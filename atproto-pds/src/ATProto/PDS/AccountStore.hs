{-# LANGUAGE ScopedTypeVariables #-}
-- | Account store abstraction for the PDS.
--
-- This module provides:
--
-- * 'Account' – persisted metadata for a single actor.
-- * 'PasswordHash' – opaque hash produced by 'hashPassword'.
-- * 'JwtKey' – 32-byte HMAC-SHA256 key used to sign session tokens.
-- * 'AuthScope' – token scope (access vs. refresh).
-- * 'RefreshTokenRecord' – server-side record of an issued refresh token.
-- * 'AccountStore' – typeclass that storage backends must implement.
--
-- The typeclass is DID-agnostic: the same interface works whether the
-- actor was created via @did:plc@ (the common case) or brought in via
-- @did:web@.  PLC rotation keys are stored as an optional extension.
--
-- = Session tokens
--
-- Access and refresh tokens are compact __HS256-signed JWTs__ following
-- the upstream AT Protocol PDS:
--
-- * __Access token__ – @typ: "at+jwt"@, @scope: "access"@, expiry 120 min.
-- * __Refresh token__ – @typ: "refresh+jwt"@, @scope: "refresh"@, expiry
--   90 days, carries a random @jti@ that is stored in the backend.
--
-- Verifying an access token requires only the symmetric 'JwtKey' from the
-- store (via 'getJwtKey') — no database look-up is needed.  Verifying a
-- refresh token additionally requires looking up the stored
-- 'RefreshTokenRecord' by @jti@ to check that it has not been revoked.
--
-- Each backend stores a single 32-byte 'JwtKey' (accessible via
-- 'getJwtKey').  The file-system backend persists the key to
-- @basedir\/jwt.key@; the in-memory backend generates a fresh key at
-- construction time.
--
-- = Pattern
--
-- The design follows the same typeclass-per-capability pattern as
-- 'ATProto.PDS.Storage.BlockStore' and 'ATProto.PDS.Storage.BlobStore':
-- one class, multiple concrete backends.  In-memory and file-system
-- backends are provided in sub-modules.
module ATProto.PDS.AccountStore
  ( -- * Data types
    Account (..)
  , PasswordHash (..)
  , AuthScope (..)
  , RefreshTokenRecord (..)
    -- * JWT key
  , JwtKey (..)
  , generateJwtKey
    -- * Password helpers
  , hashPassword
  , checkPassword
    -- * Session token helpers
  , createAccessToken
  , createRefreshToken
  , verifyAccessToken
  , verifyRefreshToken
    -- * Typeclass
  , AccountStore (..)
  ) where

import           Control.Monad           (when)
import           Control.Monad.IO.Class  (MonadIO (..))
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Key          as AesonKey
import           Data.Aeson.Types        (parseEither)
import qualified Data.ByteArray          as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import qualified Crypto.KDF.BCrypt       as BCrypt
import qualified Crypto.MAC.HMAC         as HMAC
import           Crypto.Hash.Algorithms  (SHA256 (..))
import           Crypto.Random           (getRandomBytes)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Data.Time.Clock         (UTCTime)
import           Data.Time.Clock.POSIX   (getPOSIXTime, posixSecondsToUTCTime)

import           ATProto.Crypto.Types    (PrivKey)
import           ATProto.Syntax.DID      (DID, parseDID, unDID)
import           ATProto.Syntax.Handle   (Handle)

-- ---------------------------------------------------------------------------
-- Data types
-- ---------------------------------------------------------------------------

-- | Persisted metadata for a single PDS account.
data Account = Account
  { accountDid         :: !DID
    -- ^ The account's decentralised identifier.
  , accountHandle      :: !Handle
    -- ^ Human-readable handle (e.g. @alice.example.com@).
  , accountEmail       :: !(Maybe T.Text)
    -- ^ Contact email address, if collected.
  , accountCreatedAt   :: !UTCTime
    -- ^ UTC timestamp of account creation.
  , accountDeactivated :: !Bool
    -- ^ 'True' when the account is deactivated (e.g. during DID migration).
  } deriving (Eq, Show)

-- | An opaque password hash produced by 'hashPassword'.
--
-- Encoded as bcrypt output base64, so it can be round-tripped through a
-- plain-text file without extra serialisation.
newtype PasswordHash = PasswordHash { unPasswordHash :: T.Text }
  deriving (Eq, Show)

-- | A 32-byte HMAC-SHA256 key used to sign session JWTs.
newtype JwtKey = JwtKey { unJwtKey :: BS.ByteString }
  deriving (Eq)

-- | Scope of a session token.
data AuthScope
  = AccessScope   -- ^ Short-lived access token (@scope: "access"@).
  | RefreshScope  -- ^ Longer-lived refresh token (@scope: "refresh"@).
  deriving (Eq, Show)

-- | A server-side record of an issued refresh token.
--
-- Stored by the backend so that the token can be individually revoked
-- by @jti@.  'rtrExpiresAt' allows backends to purge expired records.
data RefreshTokenRecord = RefreshTokenRecord
  { rtrJti       :: !T.Text
    -- ^ Unique JWT identifier (@jti@ claim).
  , rtrDid       :: !DID
    -- ^ The DID this token was issued to.
  , rtrExpiresAt :: !UTCTime
    -- ^ When the token expires; used for cleanup.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- JWT key
-- ---------------------------------------------------------------------------

-- | Generate a fresh 32-byte HMAC-SHA256 key.
generateJwtKey :: IO JwtKey
generateJwtKey = JwtKey <$> getRandomBytes 32

-- ---------------------------------------------------------------------------
-- Password helpers
-- ---------------------------------------------------------------------------

-- | bcrypt cost factor used for password hashing.
bcryptCost :: Int
bcryptCost = 10

-- | Hash a plaintext password for storage using bcrypt.
hashPassword :: T.Text -> IO PasswordHash
hashPassword pwd = do
  raw :: BS.ByteString <- BCrypt.hashPassword bcryptCost (TE.encodeUtf8 pwd)
  return (PasswordHash (TE.decodeUtf8 (BAE.convertToBase BAE.Base64 raw)))

-- | Verify a plaintext password against a stored 'PasswordHash'.
--
-- Returns 'True' if and only if the password matches.
checkPassword :: T.Text -> PasswordHash -> Bool
checkPassword pwd (PasswordHash encoded) =
  let raw :: Either String BS.ByteString
      raw = BAE.convertFromBase BAE.Base64 (TE.encodeUtf8 encoded)
  in case raw of
    Left _  -> False
    Right x -> BCrypt.validatePassword (TE.encodeUtf8 pwd) x

-- ---------------------------------------------------------------------------
-- Session token helpers
-- ---------------------------------------------------------------------------

-- | Access token lifetime: 120 minutes.
accessTokenTTL :: Int
accessTokenTTL = 120 * 60

-- | Refresh token lifetime: 90 days.
refreshTokenTTL :: Int
refreshTokenTTL = 90 * 24 * 60 * 60

-- | Create a signed access token JWT (HS256, @typ: "at+jwt"@, expires in
-- 120 minutes).
--
-- The token carries @scope: "access"@, @sub@ (DID), @aud@ (service DID),
-- @iat@, and @exp@ claims.
createAccessToken
  :: JwtKey
  -> T.Text  -- ^ Audience: the service DID (@aud@ claim).
  -> DID     -- ^ Subject DID.
  -> IO T.Text
createAccessToken key aud did = do
  now <- posixNow
  let expT    = now + accessTokenTTL
      header  = "{\"alg\":\"HS256\",\"typ\":\"at+jwt\"}"
      payload = BL.toStrict $ Aeson.encode $ Aeson.object
        [ "scope" Aeson..= ("access" :: T.Text)
        , "sub"   Aeson..= unDID did
        , "aud"   Aeson..= aud
        , "iat"   Aeson..= now
        , "exp"   Aeson..= expT
        ]
  return $ TE.decodeUtf8 $ compactJwt key header payload

-- | Create a signed refresh token JWT (HS256, @typ: "refresh+jwt"@, expires
-- in 90 days).
--
-- Returns the compact JWT text alongside a 'RefreshTokenRecord' that the
-- caller should persist via 'storeRefreshToken'.
createRefreshToken
  :: JwtKey
  -> T.Text  -- ^ Audience: the service DID (@aud@ claim).
  -> DID     -- ^ Subject DID.
  -> IO (T.Text, RefreshTokenRecord)
createRefreshToken key aud did = do
  now <- posixNow
  jti <- randomHex 16
  let expT      = now + refreshTokenTTL
      expiresAt = posixSecondsToUTCTime (fromIntegral expT)
      header    = "{\"alg\":\"HS256\",\"typ\":\"refresh+jwt\"}"
      payload   = BL.toStrict $ Aeson.encode $ Aeson.object
        [ "scope" Aeson..= ("refresh" :: T.Text)
        , "sub"   Aeson..= unDID did
        , "aud"   Aeson..= aud
        , "iat"   Aeson..= now
        , "exp"   Aeson..= expT
        , "jti"   Aeson..= jti
        ]
      record = RefreshTokenRecord
        { rtrJti       = jti
        , rtrDid       = did
        , rtrExpiresAt = expiresAt
        }
  return (TE.decodeUtf8 (compactJwt key header payload), record)

-- | Verify an access token JWT.
--
-- Checks the HS256 signature, confirms @scope: "access"@, and rejects
-- expired tokens.  Returns the embedded 'DID' on success.
verifyAccessToken :: JwtKey -> T.Text -> IO (Either T.Text DID)
verifyAccessToken key jwt = do
  now <- posixNow
  return $ do
    (_, payloadB64) <- checkSignature key (TE.encodeUtf8 jwt)
    obj             <- decodePayload payloadB64
    scope           <- getField obj "scope" :: Either T.Text T.Text
    when (scope /= "access") (Left "token is not an access token")
    expT            <- getField obj "exp" :: Either T.Text Int
    when (expT <= now) (Left "access token has expired")
    sub             <- getField obj "sub" :: Either T.Text T.Text
    mapLeft T.pack (parseDID sub)

-- | Verify a refresh token JWT.
--
-- Checks the HS256 signature, confirms @scope: "refresh"@, and rejects
-- expired tokens.  Returns @(did, jti)@ on success; the caller should
-- then call 'getRefreshToken' to confirm the token has not been revoked.
verifyRefreshToken :: JwtKey -> T.Text -> IO (Either T.Text (DID, T.Text))
verifyRefreshToken key jwt = do
  now <- posixNow
  return $ do
    (_, payloadB64) <- checkSignature key (TE.encodeUtf8 jwt)
    obj             <- decodePayload payloadB64
    scope           <- getField obj "scope" :: Either T.Text T.Text
    when (scope /= "refresh") (Left "token is not a refresh token")
    expT            <- getField obj "exp" :: Either T.Text Int
    when (expT <= now) (Left "refresh token has expired")
    sub             <- getField obj "sub" :: Either T.Text T.Text
    jti             <- getField obj "jti" :: Either T.Text T.Text
    did             <- mapLeft T.pack (parseDID sub)
    return (did, jti)

-- ---------------------------------------------------------------------------
-- Typeclass
-- ---------------------------------------------------------------------------

-- | Abstraction over account persistence backends.
--
-- Backends must implement 'createAccount', 'getAccount', 'updateAccount',
-- 'deleteAccount', 'storePassword', 'getPasswordHash', 'getSigningKey',
-- 'storePlcRotationKey', 'getPlcRotationKey', 'getJwtKey',
-- 'storeRefreshToken', 'getRefreshToken', 'revokeRefreshToken', and
-- 'revokeRefreshTokensByDid'.
--
-- = Invariants
--
-- * 'createAccount' stores the account unconditionally; calling it twice
--   with the same DID replaces the first entry.
-- * 'getAccount' returns 'Nothing' for unknown DIDs.
class AccountStore s where
  -- | Persist a new account.
  --
  -- The signing key is stored alongside the account so the PDS can
  -- sign commits on behalf of the actor.
  createAccount
    :: MonadIO m
    => s
    -> Account    -- ^ Account metadata
    -> PrivKey    -- ^ Actor's signing key
    -> m ()

  -- | Retrieve an account by DID, or 'Nothing' if unknown.
  getAccount
    :: MonadIO m
    => s
    -> DID
    -> m (Maybe Account)

  -- | Update the mutable fields of an existing account.
  --
  -- The DID and creation timestamp are immutable; only handle, email,
  -- and deactivation status may change.
  updateAccount
    :: MonadIO m
    => s
    -> Account
    -> m ()

  -- | Delete an account and all associated data (key, password, sessions).
  deleteAccount
    :: MonadIO m
    => s
    -> DID
    -> m ()

  -- | Store a password hash for a DID.
  storePassword
    :: MonadIO m
    => s
    -> DID
    -> PasswordHash
    -> m ()

  -- | Retrieve the stored password hash for a DID, or 'Nothing'.
  getPasswordHash
    :: MonadIO m
    => s
    -> DID
    -> m (Maybe PasswordHash)

  -- | Retrieve the signing key for a DID, or 'Nothing'.
  getSigningKey
    :: MonadIO m
    => s
    -> DID
    -> m (Maybe PrivKey)

  -- | Get the 'JwtKey' used to sign and verify session tokens.
  getJwtKey
    :: MonadIO m
    => s
    -> m JwtKey

  -- | Record a newly issued refresh token.
  storeRefreshToken
    :: MonadIO m
    => s
    -> RefreshTokenRecord
    -> m ()

  -- | Retrieve a refresh token record by @jti@, or 'Nothing' if unknown
  -- or already revoked.
  getRefreshToken
    :: MonadIO m
    => s
    -> T.Text     -- ^ @jti@ claim from the token.
    -> m (Maybe RefreshTokenRecord)

  -- | Revoke a single refresh token by @jti@.
  revokeRefreshToken
    :: MonadIO m
    => s
    -> T.Text     -- ^ @jti@ claim.
    -> m ()

  -- | Revoke all refresh tokens for a given DID.
  revokeRefreshTokensByDid
    :: MonadIO m
    => s
    -> DID
    -> m ()

  -- | Store a PLC rotation key for a DID.
  --
  -- PLC rotation keys authorise updates to a @did:plc@ document.  This
  -- is a no-op for deployments that do not use @did:plc@.
  storePlcRotationKey
    :: MonadIO m
    => s
    -> DID
    -> PrivKey
    -> m ()

  -- | Retrieve the stored PLC rotation key for a DID, or 'Nothing'.
  --
  -- Returns 'Nothing' for DIDs that have no stored rotation key (e.g.
  -- @did:web@ accounts, or backends that do not support PLC).
  getPlcRotationKey
    :: MonadIO m
    => s
    -> DID
    -> m (Maybe PrivKey)

-- ---------------------------------------------------------------------------
-- Internal JWT helpers
-- ---------------------------------------------------------------------------

-- | Produce a compact JWT: @headerB64url.payloadB64url.sigB64url@.
compactJwt :: JwtKey -> BS.ByteString -> BS.ByteString -> BS.ByteString
compactJwt key headerJson payloadJson =
  let headerB64  = b64url headerJson
      payloadB64 = b64url payloadJson
      sigInput   = headerB64 <> "." <> payloadB64
      sig        = hmacSHA256 key sigInput
      sigB64     = b64url sig
  in headerB64 <> "." <> payloadB64 <> "." <> sigB64

-- | Verify the HS256 signature of a compact JWT.
--
-- Returns @(headerB64url, payloadB64url)@ on success so callers can
-- decode the payload without re-splitting.
checkSignature
  :: JwtKey
  -> BS.ByteString       -- ^ Raw JWT bytes (ASCII).
  -> Either T.Text (BS.ByteString, BS.ByteString)
checkSignature key jwtBytes =
  case BC.split '.' jwtBytes of
    [hdrB64, plB64, sigB64] ->
      let sigInput    = hdrB64 <> "." <> plB64
          expected    = hmacSHA256 key sigInput
      in case BAE.convertFromBase BAE.Base64URLUnpadded sigB64 of
           Left _       -> Left "malformed JWT: bad signature encoding"
           Right actual ->
             if BA.constEq (actual :: BS.ByteString) expected
               then Right (hdrB64, plB64)
               else Left "invalid JWT signature"
    _ -> Left "malformed JWT: expected 3 dot-separated segments"

-- | Base64url-decode a payload segment and parse it as a JSON object.
decodePayload :: BS.ByteString -> Either T.Text Aeson.Object
decodePayload b64 = do
  bytes <- mapLeft T.pack (BAE.convertFromBase BAE.Base64URLUnpadded b64)
  mapLeft T.pack (Aeson.eitherDecodeStrict bytes)

-- | Look up a field in a JSON object, returning an error on missing or
-- wrongly-typed values.
getField :: Aeson.FromJSON a => Aeson.Object -> T.Text -> Either T.Text a
getField obj k =
  mapLeft T.pack $
    parseEither (Aeson..: AesonKey.fromText k) obj

-- | Compute HMAC-SHA256 over @msg@ using @key@.
hmacSHA256 :: JwtKey -> BS.ByteString -> BS.ByteString
hmacSHA256 (JwtKey keyBytes) msg =
  BA.convert (HMAC.hmac keyBytes msg :: HMAC.HMAC SHA256)

-- | Base64url-encode without padding.
b64url :: BS.ByteString -> BS.ByteString
b64url = BAE.convertToBase BAE.Base64URLUnpadded

-- | Get current POSIX time as an 'Int' (seconds since epoch).
posixNow :: IO Int
posixNow = round <$> getPOSIXTime

-- | Generate @n@ random bytes encoded as lowercase hex.
randomHex :: Int -> IO T.Text
randomHex n = do
  bs <- getRandomBytes n :: IO BS.ByteString
  return $ TE.decodeUtf8 $ BAE.convertToBase BAE.Base16 bs

-- | Map over the 'Left' side of an 'Either'.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right c) = Right c

