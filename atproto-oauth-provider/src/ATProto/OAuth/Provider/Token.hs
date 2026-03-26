{-# LANGUAGE ScopedTypeVariables #-}
-- | Access token creation and verification for the ATProto OAuth provider.
--
-- Ported from the upstream TypeScript reference implementation
-- (@atproto\/oauth-provider\/src\/signer\/signer.ts@).
--
-- Creates and verifies DPoP-bound access tokens as signed JWTs with
-- @typ: at+jwt@ (RFC 9068).  The @cnf.jkt@ claim binds the token to
-- a specific DPoP key.
--
-- = Security properties
--
-- * Access tokens are signed with ES256 (EC P-256) using the provider's
--   private signing key.
-- * The @cnf.jkt@ claim contains the SHA-256 JWK thumbprint of the
--   client's DPoP key, preventing token theft.
-- * Tokens include @iss@, @sub@, @aud@, @jti@, @exp@, @iat@, and @scope@
--   claims.
-- * Token verification checks the issuer, typ, expiry, and signature.
module ATProto.OAuth.Provider.Token
  ( -- * Key management
    SigningKey
  , generateSigningKey
    -- * Token creation
  , CreateTokenParams (..)
  , createAccessToken
    -- * Token verification
  , verifyAccessToken
  ) where

import           Control.Lens              (set, view, toListOf)
import qualified Crypto.Random             as Random
import           Crypto.JOSE.Compact       (encodeCompact, decodeCompact)
import           Crypto.JOSE.Error         (Error, runJOSE)
import           Crypto.JOSE.Header        (HeaderParam (..), HasTyp (typ))
import           Crypto.JOSE.JWK           (Crv (..), JWK,
                                            KeyMaterialGenParam (..), genJWK)
import           Crypto.JOSE.JWS           (Alg (..), CompactJWS, JWSHeader,
                                            RequiredProtection (..),
                                            newJWSHeaderProtected, signJWS,
                                            verifyJWS', signatures,
                                            header)
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Types          as AesonTypes
import           Data.ByteArray.Encoding   (Base (..), convertToBase)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Functor.Identity     as Identity
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Time.Clock.POSIX     (getPOSIXTime)

import           ATProto.OAuth.Provider.Types (AccessTokenPayload (..), ProviderError (..))

-- ---------------------------------------------------------------------------
-- Key management
-- ---------------------------------------------------------------------------

-- | The provider's signing key for access tokens.
--
-- An EC P-256 keypair used to sign @at+jwt@ access tokens.
newtype SigningKey = SigningKey JWK

-- | Generate a fresh signing key using the OS CSPRNG.
generateSigningKey :: IO SigningKey
generateSigningKey = SigningKey <$> genJWK (ECGenParam P_256)

-- ---------------------------------------------------------------------------
-- Token creation
-- ---------------------------------------------------------------------------

-- | Parameters for creating an access token.
data CreateTokenParams = CreateTokenParams
  { ctpSub      :: T.Text
    -- ^ Subject DID.
  , ctpAud      :: T.Text
    -- ^ Audience (resource server URL).
  , ctpScope    :: T.Text
    -- ^ Granted scopes.
  , ctpClientId :: Maybe T.Text
    -- ^ Client identifier.
  , ctpCnfJkt   :: Maybe T.Text
    -- ^ DPoP JWK thumbprint for binding.
  , ctpLifetime :: Int
    -- ^ Token lifetime in seconds.
  } deriving (Eq, Show)

-- | Create a signed access token.
--
-- The token is a compact JWS with @typ: at+jwt@ and @alg: ES256@,
-- signed with the provider's signing key.
createAccessToken
  :: SigningKey
  -> T.Text
  -- ^ Issuer URL.
  -> CreateTokenParams
  -> IO (Either String T.Text)
createAccessToken (SigningKey signingKey) issuer params = do
  jti <- generateTokenId
  iat <- fmap (floor :: Double -> Int) (fmap realToFrac getPOSIXTime)
  let expiry = iat + ctpLifetime params

  -- Build the payload.
  let payloadValue = Aeson.object $
        [ "iss"   Aeson..= issuer
        , "sub"   Aeson..= ctpSub params
        , "aud"   Aeson..= ctpAud params
        , "jti"   Aeson..= jti
        , "scope" Aeson..= ctpScope params
        , "iat"   Aeson..= iat
        , "exp"   Aeson..= expiry
        ] ++
        maybe [] (\cid -> ["client_id" Aeson..= cid]) (ctpClientId params) ++
        maybe [] (\jkt -> ["cnf" Aeson..= Aeson.object ["jkt" Aeson..= jkt]])
                 (ctpCnfJkt params)

      payloadBytes = BL.toStrict (Aeson.encode payloadValue)

  -- Build the header.
  let hdr0 = newJWSHeaderProtected ES256 :: JWSHeader RequiredProtection
      hdr1 = set typ (Just (HeaderParam RequiredProtection "at+jwt")) hdr0

  -- Sign.
  result <- runJOSE (signJWS payloadBytes (Identity.Identity (hdr1, signingKey)))
              :: IO (Either Error (CompactJWS JWSHeader))
  case result of
    Left err -> return (Left (show err))
    Right jws ->
      let compactBytes = BL.toStrict (encodeCompact jws)
      in  return (Right (TE.decodeUtf8Lenient compactBytes))

-- | Verify an access token and extract the payload.
--
-- Checks:
-- * The token is a valid compact JWS.
-- * The @typ@ header is @at+jwt@.
-- * The signature verifies against the signing key.
-- * The @iss@ claim matches the expected issuer.
-- * The token has not expired.
verifyAccessToken
  :: SigningKey
  -> T.Text
  -- ^ Expected issuer URL.
  -> T.Text
  -- ^ The compact JWS token.
  -> IO (Either ProviderError AccessTokenPayload)
verifyAccessToken (SigningKey signingKey) expectedIssuer token = do
  now <- fmap (floor :: Double -> Int) (fmap realToFrac getPOSIXTime)

  -- Decode the compact JWS.
  let compactBytes = BL.fromStrict (TE.encodeUtf8 token)
  decResult <- runJOSE (decodeCompact compactBytes)
                 :: IO (Either Error (CompactJWS JWSHeader))
  case decResult of
    Left _err -> return (Left (InvalidToken "Malformed token"))
    Right jws -> do
      -- Verify typ header.
      case verifyTypHeader jws of
        Left err -> return (Left err)
        Right () -> do
          -- Verify signature and get payload bytes.
          verResult <- runJOSE (verifyJWS' signingKey jws)
                         :: IO (Either Error BS.ByteString)
          case verResult of
            Left _err -> return (Left (InvalidToken "Token signature verification failed"))
            Right payloadBs ->
              -- Parse payload.
              case Aeson.eitherDecode' (BL.fromStrict payloadBs) of
                Left err -> return (Left (InvalidToken (T.pack ("Bad payload: " ++ err))))
                Right obj -> return (parseAndValidatePayload obj expectedIssuer now)

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Verify the typ header is @at+jwt@.
verifyTypHeader :: CompactJWS JWSHeader -> Either ProviderError ()
verifyTypHeader jws =
  case toListOf signatures jws of
    [] -> Left (InvalidToken "No signatures in token")
    (sig:_) ->
      case view typ (view header sig) of
        Nothing -> Left (InvalidToken "Token missing \"typ\" header")
        Just (HeaderParam _ t)
          | t /= "at+jwt" -> Left (InvalidToken ("Wrong token typ: " <> T.pack (show t)))
          | otherwise -> Right ()

-- | Parse and validate the token payload.
parseAndValidatePayload
  :: Aeson.Object
  -> T.Text     -- expected issuer
  -> Int        -- current time
  -> Either ProviderError AccessTokenPayload
parseAndValidatePayload obj expectedIssuer now = do
  let getText k = case AesonTypes.parseMaybe (Aeson..: k) obj of
                    Just (Aeson.String t) -> Just t
                    _                     -> Nothing
      getInt  k = case AesonTypes.parseMaybe (Aeson..: k) obj of
                    Just (Aeson.Number n) -> Just (round n :: Int)
                    _                     -> Nothing

  iss <- maybe (Left (InvalidToken "Missing \"iss\"")) Right (getText "iss")
  sub <- maybe (Left (InvalidToken "Missing \"sub\"")) Right (getText "sub")
  aud <- maybe (Left (InvalidToken "Missing \"aud\"")) Right (getText "aud")
  jti <- maybe (Left (InvalidToken "Missing \"jti\"")) Right (getText "jti")
  scp <- maybe (Left (InvalidToken "Missing \"scope\"")) Right (getText "scope")
  iat <- maybe (Left (InvalidToken "Missing \"iat\"")) Right (getInt "iat")
  expT <- maybe (Left (InvalidToken "Missing \"exp\"")) Right (getInt "exp")

  -- Verify issuer.
  if iss /= expectedIssuer
    then Left (InvalidToken ("Issuer mismatch: " <> iss <> " /= " <> expectedIssuer))
    else Right ()

  -- Verify expiry.
  if expT <= now
    then Left (InvalidToken "Token expired")
    else Right ()

  -- Extract optional claims.
  let clientId = getText "client_id"
      cnfJkt   = extractCnfJkt obj

  Right AccessTokenPayload
    { atpIss      = iss
    , atpSub      = sub
    , atpAud      = aud
    , atpJti      = jti
    , atpScope    = scp
    , atpClientId = clientId
    , atpIat      = iat
    , atpExp      = expT
    , atpCnfJkt   = cnfJkt
    }

-- | Extract @cnf.jkt@ from the token payload.
extractCnfJkt :: Aeson.Object -> Maybe T.Text
extractCnfJkt obj =
  case AesonTypes.parseMaybe (Aeson..: "cnf") obj of
    Just (Aeson.Object cnf) ->
      case AesonTypes.parseMaybe (Aeson..: "jkt") cnf of
        Just (Aeson.String jkt) -> Just jkt
        _                       -> Nothing
    _ -> Nothing

-- | Generate a random token ID (16 bytes hex = 32 chars).
generateTokenId :: IO T.Text
generateTokenId = do
  bytes <- Random.getRandomBytes 16 :: IO BS.ByteString
  let hexBytes = convertToBase Base16 bytes :: BS.ByteString
  return (TE.decodeUtf8Lenient hexBytes)
