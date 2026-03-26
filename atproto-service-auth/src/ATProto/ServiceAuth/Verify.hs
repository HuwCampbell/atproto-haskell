-- | Verify AT Protocol inter-service authentication JWTs.
--
-- This module decodes a compact JWS token, validates the claims
-- (expiry, audience, lexicon method), and verifies the ECDSA signature
-- against a public key obtained via a caller-supplied callback.
--
-- If the initial signature check fails the callback is invoked a second
-- time with @forceRefresh = True@, allowing the caller to bypass any
-- key cache and fetch the latest signing key from the DID document.
module ATProto.ServiceAuth.Verify
  ( ServiceJwtPayload (..)
  , ServiceAuthError (..)
  , verifyServiceJwt
  ) where

import           Control.Monad               (when, unless)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Either  (runEitherT, newEitherT, left, hoistEither, firstEitherT)

import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Key              as AesonKey
import qualified Data.Aeson.Types            as Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64.URL  as Base64URL
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import           Data.Foldable               (for_)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Data.Time.Clock.POSIX       (getPOSIXTime)

import           ATProto.Crypto.EC           (verify)
import           ATProto.Crypto.Types        (Signature (..), SigStrictness (..))
import           ATProto.DID                 (DidResolver (..), CachingDidResolver (..), ResolveError)
import           ATProto.Repo.Verify.Key     (resolveAtprotoKey)
import           ATProto.Repo.Verify.Types   (VerifyError)


-- | The validated payload of a service authentication JWT.
data ServiceJwtPayload = ServiceJwtPayload
  { payloadIss :: T.Text
    -- ^ Issuer DID.
  , payloadAud :: T.Text
    -- ^ Audience DID.
  , payloadExp :: Int
    -- ^ Expiry (seconds since epoch).
  , payloadLxm :: Maybe T.Text
    -- ^ Lexicon method NSID, if present.
  } deriving (Eq, Show)

-- | Errors that can occur during service JWT verification.
data ServiceAuthError
  = BadJwt String
    -- ^ Malformed token (bad structure, unknown algorithm, etc.).
  | JwtExpired
    -- ^ The token has expired.
  | BadJwtAudience
    -- ^ The audience claim does not match the expected DID.
  | BadJwtLexiconMethod
    -- ^ The lexicon method claim does not match the expected NSID.
  | BadJwtSignature
    -- ^ The ECDSA signature is invalid.
  | BadJwtIss ResolveError
    -- ^ The issuer key could not be resolved.
  | BadSigningKeys VerifyError
    -- ^ The issuer key could not be resolved.
  deriving (Eq, Show)

-- | Verify a service authentication JWT.
--
-- The @getSigningKey@ callback is called with the issuer DID from the
-- token and a @forceRefresh@ flag.  If the signature does not verify on
-- the first attempt, the callback is invoked again with
-- @forceRefresh = True@ to allow cache invalidation.
verifyServiceJwt
  :: CachingDidResolver resolver
  -- ^ How to retrieve a DID
  => T.Text
  -- ^ The compact JWS token.
  -> Maybe T.Text
  -- ^ Expected audience DID.  Pass 'Nothing' to skip the audience check.
  -> Maybe T.Text
  -- ^ Expected lexicon method.  Pass 'Nothing' to skip the lxm check.
  -> resolver
  -- ^ @getSigningKey iss forceRefresh@ – resolve the public key for
  -- an issuer DID.
  -> IO (Either ServiceAuthError ServiceJwtPayload)
verifyServiceJwt jwt ownDid lxm resolver = runEitherT $ do
  (headerB64, payloadB64, sigB64) <-
    hoistEither $ splitJwt (TE.encodeUtf8 jwt)

  (iss, aud, expT, mLxm) <-
    hoistEither $ parseHeaderAndPayload headerB64 payloadB64

  now <- round <$> lift getPOSIXTime
  when (expT <= now) $
    left JwtExpired

  -- Check audience
  for_ ownDid $ \expected ->
    when (aud /= expected) $
      left BadJwtAudience

  for_ lxm $ \expected ->
    unless (Just expected == mLxm) $
      left BadJwtLexiconMethod

  -- Decode signature
  sigBytes <-
    hoistEither $
      mapLeft (\e -> BadJwt ("bad signature encoding: " ++ e)) $
        base64urlDecode sigB64

  let sigInput = headerB64 <> "." <> payloadB64
      sig      = Signature sigBytes
      payload  = ServiceJwtPayload iss aud expT mLxm

  -- First attempt with cached key
  didDocument <-
    firstEitherT BadJwtIss . newEitherT $
      resolve resolver iss

  pubKey <-
    either (left . BadSigningKeys) pure $
      resolveAtprotoKey didDocument

  if verify Strict pubKey sigInput sig then
    return payload
  else do
    -- Try refreshing the DID document in case it was updated
    didDocument2 <-
      firstEitherT BadJwtIss . newEitherT $
        refreshResolve resolver iss

    pubKey2 <-
      either (left . BadSigningKeys) pure $
        resolveAtprotoKey didDocument2

    if verify Strict pubKey2 sigInput sig then
      return payload
    else
      left BadJwtSignature

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Split a JWT into its three base64url-encoded segments.
splitJwt :: BS.ByteString -> Either ServiceAuthError (BS.ByteString, BS.ByteString, BS.ByteString)
splitJwt bs =
  case BC.split '.' bs of
    [h, p, s] -> Right (h, p, s)
    _         -> Left (BadJwt "expected three dot-separated segments")

-- | Decode and validate the JWT header and payload.
--
-- Returns @(iss, aud, exp, maybeLxm)@ on success.
parseHeaderAndPayload
  :: BS.ByteString
  -> BS.ByteString
  -> Either ServiceAuthError (T.Text, T.Text, Int, Maybe T.Text)
parseHeaderAndPayload headerB64 payloadB64 = do
  headerBytes  <- mapLeft (\e -> BadJwt ("header base64: " ++ e)) (base64urlDecode headerB64)
  payloadBytes <- mapLeft (\e -> BadJwt ("payload base64: " ++ e)) (base64urlDecode payloadB64)

  headerObj  <- mapLeft (\e -> BadJwt ("header JSON: " ++ e))
                  (Aeson.eitherDecode' (BL.fromStrict headerBytes))
  payloadObj <- mapLeft (\e -> BadJwt ("payload JSON: " ++ e))
                  (Aeson.eitherDecode' (BL.fromStrict payloadBytes))

  validateHeader headerObj
  parsePayload payloadObj

-- | Validate the JWT header: check @alg@ is ES256 or ES256K, reject
-- @dpop+jwt@ and @at+jwt@ token types.
validateHeader :: Aeson.Object -> Either ServiceAuthError ()
validateHeader obj = do
  alg <- mapLeft (\e -> BadJwt ("header alg: " ++ e))
           (Aeson.parseEither (Aeson..: "alg") obj :: Either String T.Text)
  case alg of
    "ES256"  -> return ()
    "ES256K" -> return ()
    _        -> Left (BadJwt ("unsupported algorithm: " ++ T.unpack alg))

  let mTyp = Aeson.parseMaybe (Aeson..: "typ") obj :: Maybe T.Text
  case mTyp of
    Just "dpop+jwt" -> Left (BadJwt "dpop+jwt tokens are not accepted")
    Just "at+jwt"   -> Left (BadJwt "at+jwt tokens are not accepted")
    _               -> return ()

-- | Extract claims from the JWT payload.
parsePayload :: Aeson.Object -> Either ServiceAuthError (T.Text, T.Text, Int, Maybe T.Text)
parsePayload obj = do
  iss <- field "iss"
  aud <- field "aud"
  expT <- mapLeft (\e -> BadJwt ("payload exp: " ++ e))
            (Aeson.parseEither (Aeson..: "exp") obj :: Either String Int)
  let mLxm = Aeson.parseMaybe (Aeson..: "lxm") obj :: Maybe T.Text
  return (iss, aud, expT, mLxm)
  where
    field :: T.Text -> Either ServiceAuthError T.Text
    field k = mapLeft (\e -> BadJwt ("payload " ++ T.unpack k ++ ": " ++ e))
                (Aeson.parseEither (Aeson..: (AesonKey.fromText k)) obj)

-- | Base64url decode, adding padding if necessary.
base64urlDecode :: BS.ByteString -> Either String BS.ByteString
base64urlDecode bs =
  let remainder = BS.length bs `mod` 4
      padded = case remainder of
        2 -> bs <> "=="
        3 -> bs <> "="
        _ -> bs
  in Base64URL.decode padded

-- | Map over the 'Left' side of an 'Either'.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right c) = Right c
