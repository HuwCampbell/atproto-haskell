-- | Storage type classes for the PDS.
--
-- The PDS uses type classes to abstract over storage backends:
--
-- * 'BlockStore'      – content-addressed block storage (CID → bytes).
-- * 'RepoStore'       – repository metadata (DID → head commit CID).
-- * 'BlobStore'       – binary blob storage (CID → bytes).
-- * 'PreferenceStore' – per-user preferences (DID → JSON bytes).
-- * 'AccountStore'    – account metadata (DID → account info).
--
-- Implement these for your preferred backend.  The library ships with
-- an in-memory implementation ('ATProto.PDS.Storage.InMemory') and a
-- file-system implementation ('ATProto.PDS.Storage.FileSystem').
module ATProto.PDS.Storage
  ( -- * Block storage
    BlockStore (..)
    -- * Repository metadata
  , RepoStore (..)
    -- * Blob storage
  , BlobStore (..)
    -- * Preference storage
  , PreferenceStore (..)
    -- * Account storage
  , AccountInfo (..)
  , AccountStore (..)
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import ATProto.Car.Cid    (CidBytes)
import ATProto.Syntax.DID (DID)

-- | Content-addressed block storage.
--
-- Each block is identified by its CID.  Implementations must be
-- safe for concurrent read/write access from multiple threads.
class BlockStore s where
  -- | Retrieve a block by its CID, or 'Nothing' if absent.
  getBlock :: s -> CidBytes -> IO (Maybe BS.ByteString)
  -- | Store a block under its CID.  Storing the same CID twice
  --   is idempotent.
  putBlock :: s -> CidBytes -> BS.ByteString -> IO ()

-- | Repository head metadata.
--
-- Maps each repository (identified by its owner DID) to the CID of
-- the latest signed commit.
class RepoStore s where
  -- | Get the head commit CID for a repository, or 'Nothing' if the
  --   repository has not been initialised.
  getRepoHead :: s -> DID -> IO (Maybe CidBytes)
  -- | Set the head commit CID for a repository.
  setRepoHead :: s -> DID -> CidBytes -> IO ()

-- | Binary blob storage (images, videos, etc.).
--
-- Keyed by CID; suitable for file-system, S3, or database backends.
class BlobStore s where
  -- | Retrieve a blob by its CID.
  getBlob  :: s -> CidBytes -> IO (Maybe BS.ByteString)
  -- | Store a blob under its CID.
  putBlob  :: s -> CidBytes -> BS.ByteString -> IO ()
  -- | List all stored blob CIDs.
  listBlobs :: s -> IO [CidBytes]

-- | Per-user preference storage.
--
-- Stores opaque JSON-encoded preferences per DID.
class PreferenceStore s where
  -- | Get the preferences for a DID, or 'Nothing' if not set.
  getPreferences :: s -> T.Text -> IO (Maybe BL.ByteString)
  -- | Set the preferences for a DID.
  putPreferences :: s -> T.Text -> BL.ByteString -> IO ()

-- | Per-account metadata.
data AccountInfo = AccountInfo
  { aiDid      :: DID
  , aiHandle   :: T.Text
  , aiPassword :: T.Text
    -- ^ Plaintext password (sufficient for a development server;
    --   a production server would store a hash).
  , aiActive   :: Bool
  } deriving (Eq, Show)

-- | Account metadata storage.
--
-- Maps DIDs to account information.  Suitable for SQLite, Postgres,
-- or other persistent backends.
class AccountStore s where
  -- | Look up an account by DID text.
  getAccount       :: s -> T.Text -> IO (Maybe AccountInfo)
  -- | Store or update an account.
  putAccount       :: s -> T.Text -> AccountInfo -> IO ()
  -- | Update an account in place.
  modifyAccount    :: s -> T.Text -> (AccountInfo -> AccountInfo) -> IO ()
  -- | Resolve a handle to a DID.
  lookupByHandle   :: s -> T.Text -> IO (Maybe DID)
  -- | Register a handle → DID mapping.
  registerHandle   :: s -> T.Text -> DID -> IO ()
  -- | List all accounts.
  listAccounts     :: s -> IO [AccountInfo]
