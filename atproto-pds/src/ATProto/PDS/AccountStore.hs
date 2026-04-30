{-# LANGUAGE ScopedTypeVariables #-}
-- | Account store abstraction for the PDS.
--
-- This module provides:
--
-- * 'Account' – persisted metadata for a single actor.
-- * 'PasswordHash' – opaque hash produced by 'hashPassword'.
-- * 'AccountStore' – typeclass that storage backends must implement.
--
-- The typeclass is DID-agnostic: the same interface works whether the
-- actor was created via @did:plc@ (the common case) or brought in via
-- @did:web@.  PLC rotation keys are stored as an optional extension:
-- backends that do not use @did:plc@ can store and retrieve them using
-- the same interface, or simply always return 'Nothing'.
--
-- = Session tokens
--
-- Session tokens are __self-describing__: they are produced by
-- 'makeSessionToken', which uses 'Web.ClientSession' to AES-encrypt the
-- actor's DID into the token itself.  Verifying a token ('verifySessionToken')
-- is therefore a pure decryption step — no database or file scan is needed.
-- This scales to any number of concurrent users without degrading.
--
-- Each backend stores a single symmetric 'Key' (accessible via
-- 'getSessionKey').  The file-system backend persists the key to
-- @basedir\/session.key@; the in-memory backend generates a fresh key at
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
    -- * Password helpers
  , hashPassword
  , checkPassword
    -- * Session token helpers
  , makeSessionToken
  , verifySessionToken
    -- * Typeclass
  , AccountStore (..)
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString        as BS
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time.Clock        (UTCTime)

import qualified Crypto.KDF.BCrypt      as BCrypt
import           Web.ClientSession      (Key, encryptIO, decrypt)

import           ATProto.Crypto.Types   (PrivKey)
import           ATProto.Syntax.DID     (DID, parseDID, unDID)
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

-- | An opaque password hash produced by 'hashPassword'.
--
-- Encoded as bcrypt output base64, so it can be round-tripped through a
-- plain-text file without extra serialisation.
newtype PasswordHash = PasswordHash { unPasswordHash :: T.Text }
  deriving (Eq, Show)

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

-- | Create a 'Web.ClientSession'-encrypted token that encodes the given DID.
--
-- The token is an AES-encrypted, HMAC-authenticated ciphertext produced by
-- 'Web.ClientSession.encryptIO'.  Because the DID is embedded in the token,
-- no server-side session table is needed for verification.
makeSessionToken :: Key -> DID -> IO T.Text
makeSessionToken key did = do
  ct <- encryptIO key (TE.encodeUtf8 (unDID did))
  return (TE.decodeUtf8 ct)

-- | Decrypt a session token and return the embedded 'DID'.
--
-- Returns 'Nothing' if the token is malformed, tampered with, or was not
-- produced by 'makeSessionToken' with the same 'Key'.
verifySessionToken :: Key -> T.Text -> Maybe DID
verifySessionToken key token =
  case decrypt key (TE.encodeUtf8 token) of
    Nothing -> Nothing
    Just bs ->
      case parseDID (TE.decodeUtf8 bs) of
        Right did -> Just did
        Left _    -> Nothing

-- ---------------------------------------------------------------------------
-- Typeclass
-- ---------------------------------------------------------------------------

-- | Abstraction over account persistence backends.
--
-- Backends must implement 'createAccount', 'getAccount', 'updateAccount',
-- 'deleteAccount', 'storePassword', 'getPasswordHash', 'getSigningKey',
-- 'storePlcRotationKey', 'getPlcRotationKey', and 'getSessionKey'.
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

  -- | Get the symmetric key used to produce and verify session tokens.
  --
  -- Tokens produced by 'makeSessionToken' with this key can be verified
  -- with 'verifySessionToken' — no session table is consulted.
  getSessionKey
    :: MonadIO m
    => s
    -> m Key

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
