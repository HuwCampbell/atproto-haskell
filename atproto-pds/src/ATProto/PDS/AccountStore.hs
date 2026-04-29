-- | Account store abstraction for the PDS.
--
-- This module provides:
--
-- * 'Account' – persisted metadata for a single actor.
-- * 'Session' – access + refresh token pair issued at login.
-- * 'PasswordHash' – opaque hash produced by 'hashPassword'.
-- * 'AccountStore' – typeclass that storage backends must implement.
--
-- The typeclass is DID-agnostic: the same interface works whether the
-- actor was created via @did:plc@ (the common case) or brought in via
-- @did:web@.  PLC rotation keys are stored as an optional extension:
-- backends that do not use @did:plc@ can store and retrieve them using
-- the same interface, or simply always return 'Nothing'.
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
  , Session (..)
  , PasswordHash (..)
    -- * Password helpers
  , hashPassword
  , checkPassword
    -- * Token helper
  , generateToken
    -- * Typeclass
  , AccountStore (..)
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString        as BS
import qualified Data.ByteArray         as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Bits              (xor)
import           Data.Time.Clock        (UTCTime)

import qualified Crypto.KDF.PBKDF2      as PBKDF2
import           Crypto.Random          (getRandomBytes)

import           ATProto.Crypto.Types   (PrivKey)
import           ATProto.Syntax.DID     (DID)
import           ATProto.Syntax.Handle  (Handle)

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

-- | A session token pair issued at login.
--
-- Access tokens are short-lived; refresh tokens are longer-lived and can
-- be exchanged for a new 'Session'.
data Session = Session
  { sessionDid        :: !DID
    -- ^ The DID this session belongs to.
  , sessionAccessJwt  :: !T.Text
    -- ^ Short-lived access token (opaque bearer token in the simple impl).
  , sessionRefreshJwt :: !T.Text
    -- ^ Longer-lived refresh token.
  } deriving (Eq, Show)

-- | An opaque password hash produced by 'hashPassword'.
--
-- Encoded as @\<salt-b64\>$\<iterations\>$\<hash-b64\>@ so it can be
-- round-tripped through a plain text file without extra serialisation.
newtype PasswordHash = PasswordHash { unPasswordHash :: T.Text }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Password helpers
-- ---------------------------------------------------------------------------

-- | Number of PBKDF2 iterations used for password hashing.
pbkdf2Iterations :: Int
pbkdf2Iterations = 100000

-- | Hash a plaintext password for storage.
--
-- Uses PBKDF2-SHA256 with a 16-byte cryptographically random salt and
-- 'pbkdf2Iterations' iterations.  The returned 'PasswordHash' encodes
-- salt, iteration count, and derived key, so 'checkPassword' needs only
-- the hash — not the original salt.
hashPassword :: T.Text -> IO PasswordHash
hashPassword pwd = do
  salt <- getRandomBytes 16 :: IO BS.ByteString
  let params  = PBKDF2.Parameters { PBKDF2.iterCounts = pbkdf2Iterations, PBKDF2.outputLength = 32 }
      digest  = PBKDF2.fastPBKDF2_SHA256 params (TE.encodeUtf8 pwd) salt :: BS.ByteString
      saltB64 = TE.decodeUtf8 (BAE.convertToBase BAE.Base64 salt :: BS.ByteString)
      hashB64 = TE.decodeUtf8 (BAE.convertToBase BAE.Base64 digest :: BS.ByteString)
      encoded = saltB64 <> "$" <> T.pack (show pbkdf2Iterations) <> "$" <> hashB64
  return (PasswordHash encoded)

-- | Verify a plaintext password against a stored 'PasswordHash'.
--
-- Returns 'True' if and only if the password matches.  Uses a
-- constant-time comparison to prevent timing side-channels.
checkPassword :: T.Text -> PasswordHash -> Bool
checkPassword pwd (PasswordHash encoded) =
  case T.splitOn "$" encoded of
    [saltB64, iterStr, hashB64] ->
      case ( BAE.convertFromBase BAE.Base64 (TE.encodeUtf8 saltB64) :: Either String BS.ByteString
           , reads (T.unpack iterStr) :: [(Int, String)]
           , BAE.convertFromBase BAE.Base64 (TE.encodeUtf8 hashB64) :: Either String BS.ByteString
           ) of
        (Right salt, [(iters, "")], Right expectedHash) ->
          let params = PBKDF2.Parameters { PBKDF2.iterCounts = iters, PBKDF2.outputLength = BS.length expectedHash }
              actual = PBKDF2.fastPBKDF2_SHA256 params (TE.encodeUtf8 pwd) salt :: BS.ByteString
          in  constantTimeEq actual expectedHash
        _ -> False
    _ -> False

-- ---------------------------------------------------------------------------
-- Token helper
-- ---------------------------------------------------------------------------

-- | Generate a cryptographically random opaque bearer token.
--
-- Produces 32 bytes from a cryptographically secure random source,
-- encoded as base64, giving a 44-character token.
generateToken :: IO T.Text
generateToken = do
  bytes <- getRandomBytes 32 :: IO BS.ByteString
  return (TE.decodeUtf8 (BAE.convertToBase BAE.Base64 bytes :: BS.ByteString))

-- ---------------------------------------------------------------------------
-- Typeclass
-- ---------------------------------------------------------------------------

-- | Abstraction over account persistence backends.
--
-- Backends must implement all methods.  The in-memory and file-system
-- backends provided in sub-modules are suitable for development and
-- simple deployments.
--
-- = Invariants
--
-- * 'createAccount' stores the account unconditionally; calling it twice
--   with the same DID replaces the first entry.
-- * 'getAccount' returns 'Nothing' for unknown DIDs.
-- * 'storeSession' / 'getSession' are keyed on the __access__ token
--   ('sessionAccessJwt').
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

  -- | Store an active session.
  storeSession
    :: MonadIO m
    => s
    -> Session
    -> m ()

  -- | Look up a session by its access token.
  getSession
    :: MonadIO m
    => s
    -> T.Text     -- ^ Access JWT / token
    -> m (Maybe Session)

  -- | Revoke a session by its access token.
  deleteSession
    :: MonadIO m
    => s
    -> T.Text     -- ^ Access JWT / token
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
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Constant-time byte-string equality (prevents timing attacks on password
-- comparison).
constantTimeEq :: BS.ByteString -> BS.ByteString -> Bool
constantTimeEq a b
  | BS.length a /= BS.length b = False
  | otherwise =
      -- BS.zipWith returns [Word8]; pack it and check all bytes are zero.
      BS.all (== 0) (BS.pack (BS.zipWith xor a b))
