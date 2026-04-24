{-# LANGUAGE TypeFamilies #-}
-- | Storage type classes for the PDS.
--
-- The PDS uses two type classes to abstract over storage backends:
--
-- * 'BlockStore' – content-addressed block storage (CID → bytes).
-- * 'RepoStore'  – repository head pointer (no DID; the store is already
--   scoped to a single actor via 'ATProto.PDS.ActorStore.ActorStore').
--
-- Implement these for your preferred backend.  The library ships with
-- an in-memory implementation ('ATProto.PDS.Storage.InMemory') and a
-- file-system implementation ('ATProto.PDS.Storage.FileSystem').
module ATProto.PDS.Storage
  ( -- * Block storage
    BlockStore (..)
    -- * Repository head
  , RepoStore (..)
    -- * Blob Storage head
  , BlobStore (..)
  ) where

import qualified Data.ByteString as BS
import Streaming.ByteString (ByteStream)

import ATProto.Car.Cid (CidBytes)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (MonadResource)

-- | Content-addressed block storage.
--
-- Instance of these types are for particular users (DIDs).
--
-- Each block is identified by its CID.  Implementations must be
-- safe for concurrent read/write access from multiple threads.
class BlockStore s where
  -- | Retrieve a block by its CID, or 'Nothing' if absent.
  getBlock :: MonadIO m => s -> CidBytes -> m (Maybe BS.ByteString)
  -- | Store a block under its CID.  Storing the same CID twice
  --   is idempotent.
  putBlock :: MonadIO m => s -> CidBytes -> BS.ByteString -> m ()

-- | Repository head pointer.
--
-- Stores the CID of the latest signed commit for the actor this store
-- is scoped to.  There is no DID parameter: the store is already
-- implicitly scoped to a single actor by
-- 'ATProto.PDS.ActorStore.openActorStore'.
class BlockStore s => RepoStore s where
  -- | Get the head commit CID, or 'Nothing' if the repository has not
  --   been initialised.
  getRepoHead :: MonadIO m => s -> m (Maybe CidBytes)
  -- | Set the head commit CID.
  setRepoHead :: MonadIO m => s -> CidBytes -> m ()

-- | Blob Store abstraction
--
-- Instance of this class type are for particular users (DIDs).
class BlobStore s where
  type TempKey s

  putTemp :: MonadResource m => s -> ByteStream m x -> m (TempKey s, x)
  streamBytes :: MonadResource m => s -> CidBytes -> ByteStream m ()
  makePermanent :: MonadIO m => s -> TempKey s -> CidBytes -> m Bool
  delete :: MonadIO m => s -> CidBytes -> m Bool
