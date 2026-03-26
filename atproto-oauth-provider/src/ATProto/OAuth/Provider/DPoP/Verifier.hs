{-# LANGUAGE ScopedTypeVariables #-}
-- | Server-side DPoP proof verification.
--
-- Ported from the upstream TypeScript reference implementation
-- (@atproto\/oauth-provider\/src\/dpop\/dpop-manager.ts@).
--
-- Verifies DPoP proof JWTs presented in the @DPoP@ HTTP header, checking:
--
-- * The proof is a valid compact JWS with @typ: dpop+jwt@.
-- * The embedded JWK is an EC key.
-- * The @htm@ claim matches the HTTP method.
-- * The @htu@ claim matches the request URL (origin + path, no query/fragment).
-- * The @jti@ claim is present (for replay detection by the caller).
-- * The @nonce@ claim matches a valid rotating nonce (if nonces are enabled).
-- * The @ath@ claim matches the SHA-256 of the access token (if one is
--   presented), or is absent (at the token endpoint).
-- * The @iat@ claim is recent (within 10 seconds + clock tolerance).
--
-- = Security properties
--
-- * The JWK thumbprint (SHA-256) of the proof's embedded key is extracted
--   and returned for binding verification against the token's @cnf.jkt@.
-- * Replay detection (jti uniqueness) is left to the caller, as it
--   requires a store (in-memory or Redis) that is application-specific.
module ATProto.OAuth.Provider.DPoP.Verifier
  ( -- * Verification
    verifyDpopProof
    -- * Helpers (exported for testing)
  , normalizeHtu
  , extractDpopHeader
  ) where

import           Control.Lens              (view, toListOf)
import qualified Crypto.Hash               as Hash
import           Crypto.Hash.Algorithms    (SHA256 (..))
import           Crypto.JOSE.Compact       (decodeCompact)
import           Crypto.JOSE.Error         (Error, runJOSE)
import           Crypto.JOSE.Header        (HasJwk (jwk), HasTyp (typ),
                                            HeaderParam (..))
import           Crypto.JOSE.JWK           (JWK, AsPublicKey (..),
                                            thumbprint)
import           Crypto.JOSE.JWS           (CompactJWS, JWSHeader,
                                            verifyJWS', signatures,
                                            header)
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Types          as AesonTypes
import qualified Data.ByteArray            as BA
import           Data.ByteArray.Encoding   (Base (..), convertToBase)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Time.Clock.POSIX     (getPOSIXTime)

import           ATProto.OAuth.Provider.Types (DpopProof (..), ProviderError (..))
import           ATProto.OAuth.Provider.DPoP.Nonce (NonceState, checkNonce, dpopNonceMaxAgeMs)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Verify a DPoP proof from request headers.
--
-- Returns 'Nothing' if no @DPoP@ header is present (anonymous request).
-- Returns @Left err@ if the header is present but the proof is invalid.
-- Returns @Right proof@ with the extracted proof on success.
--
-- The caller is responsible for replay detection (checking 'dpJti'
-- uniqueness).
verifyDpopProof
  :: T.Text
  -- ^ HTTP method (e.g. @\"POST\"@).  Case-sensitive per RFC 9110 §9.1.
  -> T.Text
  -- ^ Full request URL (will be normalised to origin + path).
  -> Map.Map T.Text T.Text
  -- ^ Request headers (case-insensitive key lookup).
  -> Maybe NonceState
  -- ^ DPoP nonce state.  Pass 'Nothing' to disable nonce validation.
  -> Maybe T.Text
  -- ^ Access token, if present.  Triggers @ath@ verification.
  -> IO (Either ProviderError (Maybe DpopProof))
verifyDpopProof httpMethod httpUrl headers mNonceState mAccessToken =
  case extractDpopHeader headers of
    Left err     -> return (Left err)
    Right Nothing -> return (Right Nothing)
    Right (Just proofJwt) -> do
      result <- verifyProofJwt proofJwt httpMethod httpUrl mNonceState mAccessToken
      case result of
        Left err    -> return (Left err)
        Right proof -> return (Right (Just proof))

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Extract the DPoP header value from headers.
--
-- Returns @Left@ if multiple DPoP headers are present (error per RFC 9449),
-- @Right Nothing@ if no header, or @Right (Just proof)@ for a single header.
extractDpopHeader
  :: Map.Map T.Text T.Text
  -> Either ProviderError (Maybe T.Text)
extractDpopHeader headers =
  case Map.lookup "dpop" lowered of
    Nothing -> Right Nothing
    Just v
      | T.null v  -> Left (InvalidDpopProof "DPoP header cannot be empty")
      | otherwise  -> Right (Just v)
  where
    lowered = Map.mapKeys T.toLower headers

-- | Verify a single DPoP proof JWT.
verifyProofJwt
  :: T.Text          -- proof JWT
  -> T.Text          -- HTTP method
  -> T.Text          -- HTTP URL
  -> Maybe NonceState
  -> Maybe T.Text    -- access token
  -> IO (Either ProviderError DpopProof)
verifyProofJwt proofJwt httpMethod httpUrl mNonceState mAccessToken = do
  now <- fmap (floor :: Double -> Int) (fmap realToFrac getPOSIXTime)

  -- Decode the compact JWS and verify signature.
  decodeResult <- decodeAndVerifyProof proofJwt
  case decodeResult of
    Left err -> return (Left (InvalidDpopProof (T.pack err)))
    Right (payloadBytes, jkt) -> do
      -- Parse the payload.
      case Aeson.eitherDecode' (BL.fromStrict payloadBytes) of
        Left err -> return (Left (InvalidDpopProof (T.pack ("Bad payload JSON: " ++ err))))
        Right obj -> verifyPayload obj jkt now

  where
    verifyPayload obj jkt now = do
      -- Extract claims.
      let getText k = case AesonTypes.parseMaybe (Aeson..: k) obj of
                        Just (Aeson.String t) -> Just t
                        _                     -> Nothing
          getInt  k = case AesonTypes.parseMaybe (Aeson..: k) obj of
                        Just (Aeson.Number n) -> Just (round n :: Int)
                        _                     -> Nothing

          mJti   = getText "jti"
          mHtm   = getText "htm"
          mHtu   = getText "htu"
          mIat   = getInt  "iat"
          mNonce = getText "nonce"
          mAth   = getText "ath"

      -- Validate jti.
      case mJti of
        Nothing -> return (Left (InvalidDpopProof "DPoP \"jti\" missing"))
        Just jti -> do
          -- Validate iat (must be recent: within 10 seconds + nonce max age tolerance).
          case mIat of
            Nothing -> return (Left (InvalidDpopProof "DPoP \"iat\" missing"))
            Just iat -> do
              let maxClockToleranceSecs = dpopNonceMaxAgeMs `div` 1000
                  maxAge = 10 + maxClockToleranceSecs
              if abs (now - iat) > maxAge
                then return (Left (InvalidDpopProof "DPoP proof too old or too far in the future"))
                else do
                  -- Validate htm.
                  case mHtm of
                    Nothing -> return (Left (InvalidDpopProof "DPoP \"htm\" missing"))
                    Just htm
                      | htm /= httpMethod ->
                          return (Left (InvalidDpopProof "DPoP \"htm\" mismatch"))
                      | otherwise -> do
                          -- Validate htu.
                          case mHtu of
                            Nothing -> return (Left (InvalidDpopProof "DPoP \"htu\" missing"))
                            Just htu -> do
                              let normalizedProofHtu = normalizeHtu htu
                                  normalizedRequestUrl = normalizeHtu httpUrl
                              if normalizedProofHtu /= normalizedRequestUrl
                                then return (Left (InvalidDpopProof "DPoP \"htu\" mismatch"))
                                else do
                                  -- Validate nonce.
                                  nonceResult <- validateNonce mNonceState mNonce
                                  case nonceResult of
                                    Left err -> return (Left err)
                                    Right () ->
                                      -- Validate ath.
                                      case validateAth mAccessToken mAth of
                                        Left err -> return (Left err)
                                        Right () ->
                                          return (Right DpopProof
                                            { dpJti = jti
                                            , dpJkt = jkt
                                            , dpHtm = htm
                                            , dpHtu = normalizedProofHtu
                                            })

-- | Decode a compact JWS, extract the embedded JWK, verify the signature,
-- and compute the JWK thumbprint.
--
-- Returns @(payloadBytes, jkt)@ on success.
decodeAndVerifyProof
  :: T.Text
  -> IO (Either String (BS.ByteString, T.Text))
decodeAndVerifyProof proofJwt = do
  -- Split into 3 dot-separated parts for basic validation.
  let parts = T.splitOn "." proofJwt
  if length parts /= 3
    then return (Left "DPoP proof is not a valid compact JWS (expected 3 segments)")
    else do
      -- Decode the compact JWS.
      let compactBytes = BL.fromStrict (TE.encodeUtf8 proofJwt)
      decResult <- runJOSE (decodeCompact compactBytes)
                     :: IO (Either Error (CompactJWS JWSHeader))
      case decResult of
        Left _err -> return (Left "Failed to decode compact JWS")
        Right jws -> do
          -- Extract the embedded key and verify typ header.
          case extractEmbeddedKey jws of
            Left err -> return (Left err)
            Right embeddedJwk -> do
              -- Verify the signature using the embedded key.
              verResult <- runJOSE (verifyJWS' embeddedJwk jws)
                             :: IO (Either Error BS.ByteString)
              case verResult of
                Left _err -> return (Left "DPoP proof signature verification failed")
                Right payloadBytes -> do
                  -- Compute JWK thumbprint.
                  let jkt = computeJwkThumbprint embeddedJwk
                  return (Right (payloadBytes, jkt))

-- | Extract the embedded JWK from the JWS protected header and verify
-- the typ is @dpop+jwt@.
extractEmbeddedKey :: CompactJWS JWSHeader -> Either String JWK
extractEmbeddedKey jws =
  case toListOf signatures jws of
    [] -> Left "No signatures in JWS"
    (sig:_) ->
      let hdr = view header sig
      in do
        -- Check typ is dpop+jwt.
        case view typ hdr of
          Nothing -> Left "DPoP proof missing \"typ\" header"
          Just (HeaderParam _ t)
            | t /= "dpop+jwt" -> Left ("DPoP proof has wrong typ: " ++ show t)
            | otherwise -> pure ()

        -- Extract embedded JWK.
        case view jwk hdr of
          Nothing -> Left "DPoP proof missing embedded \"jwk\" header"
          Just (HeaderParam _ k) ->
            -- Verify it's a public key only (no private material).
            case view asPublicKey k of
              Nothing   -> Left "DPoP proof JWK has no public key"
              Just pubK -> Right pubK

-- | Compute the SHA-256 JWK thumbprint as a base64url-unpadded string.
--
-- This matches the upstream @calculateJwkThumbprint(jwk, 'sha256')@.
computeJwkThumbprint :: JWK -> T.Text
computeJwkThumbprint k =
  let digest = view thumbprint k :: Hash.Digest SHA256
      bytes  = BA.convert digest :: BS.ByteString
  in  TE.decodeUtf8 (convertToBase Base64URLUnpadded bytes)

-- | Normalise a URL to origin + path (strip query and fragment).
--
-- Matches the upstream @normalizeHtuUrl@.
normalizeHtu :: T.Text -> T.Text
normalizeHtu url =
  -- Strip fragment (#...).
  let noFragment = T.takeWhile (/= '#') url
  -- Strip query (?...).
      noQuery    = T.takeWhile (/= '?') noFragment
  in  noQuery

-- | Validate the DPoP nonce claim.
validateNonce :: Maybe NonceState -> Maybe T.Text -> IO (Either ProviderError ())
validateNonce Nothing _ = return (Right ())
validateNonce (Just _nonceState) Nothing = return (Left UseDpopNonce)
validateNonce (Just nonceState) (Just nonce) = do
  valid <- checkNonce nonceState nonce
  if valid
    then return (Right ())
    else return (Left UseDpopNonce)

-- | Validate the DPoP @ath@ (access token hash) claim.
validateAth :: Maybe T.Text -> Maybe T.Text -> Either ProviderError ()
validateAth Nothing Nothing = Right ()
validateAth Nothing (Just _) =
  Left (InvalidDpopProof "DPoP \"ath\" claim not allowed at token endpoint")
validateAth (Just _at) Nothing =
  Left (InvalidDpopProof "DPoP \"ath\" missing for resource request")
validateAth (Just at) (Just ath) =
  let expected = computeAth at
  in  if ath == expected
        then Right ()
        else Left (InvalidDpopProof "DPoP \"ath\" mismatch")

-- | Compute the expected @ath@ value: base64url-unpadded SHA-256 of the
-- access token.
computeAth :: T.Text -> T.Text
computeAth at =
  let digest = Hash.hashWith SHA256 (TE.encodeUtf8 at) :: Hash.Digest SHA256
      bytes  = BA.convert digest :: BS.ByteString
  in  TE.decodeUtf8 (convertToBase Base64URLUnpadded bytes)
