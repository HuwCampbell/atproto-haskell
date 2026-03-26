-- | Storage type classes for the PDS.
--
-- The PDS uses two type classes to abstract over storage backends:
--
-- * 'BlockStore' – content-addressed block storage (CID → bytes).
-- * 'RepoStore'  – repository metadata (DID → head commit CID).
--
-- Implement these for your preferred backend.  The library ships with
-- an in-memory implementation ('ATProto.PDS.Storage.InMemory') and a
-- file-system implementation ('ATProto.PDS.Storage.FileSystem').
module ATProto.PDS.Storage
  ( -- * Block storage
    BlockStore (..)
    -- * Repository metadata
  , RepoStore (..)
  ) where

import qualified Data.ByteString as BS

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
