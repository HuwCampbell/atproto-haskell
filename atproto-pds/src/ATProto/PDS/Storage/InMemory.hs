{-# LANGUAGE TypeFamilies #-}
-- | In-memory storage backend.
--
-- Uses 'IORef'-wrapped 'Data.Map.Strict.Map's.  Suitable for tests and
-- short-lived processes where persistence is not required.
--
-- The module exposes two levels of API:
--
-- * 'InMemoryStore' / 'newInMemoryStore' — a global store implementing
--   the legacy 'BlockStore' and 'RepoStore' type classes (kept for
--   backward compatibility).
--
-- * 'InMemoryBackend' / 'newInMemoryBackend' — a factory that implements
--   'ActorStoreBackend', producing per-DID 'InMemoryActorStore' values
--   that implement 'ActorStorage'.  This is the preferred API for use
--   with "ATProto.PDS.Repo".
--
-- @
-- backend <- newInMemoryBackend
-- store   <- openActorStore backend did
-- Right _commit <- initRepo store privKey
-- @
module ATProto.PDS.Storage.InMemory
  ( -- * Legacy global store
    InMemoryStore
  , newInMemoryStore
    -- * Per-actor backend
  , InMemoryBackend
  , newInMemoryBackend
  , InMemoryActorStore
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.IORef

import ATProto.Car.Cid        (CidBytes)
import ATProto.Syntax.DID     (DID)
import ATProto.PDS.Storage    (BlockStore (..), RepoStore (..))
import ATProto.PDS.ActorStore (ActorStorage (..), ActorStore (..), ActorStoreBackend (..))

-- ---------------------------------------------------------------------------
-- Legacy global store
-- ---------------------------------------------------------------------------

-- | An in-memory store backed by 'IORef' 'Map's.
--
-- This type is kept for backward compatibility.  Prefer
-- 'InMemoryBackend' for new code.
data InMemoryStore = InMemoryStore
  { imsBlocks :: IORef (Map.Map CidBytes BS.ByteString)
  , imsHeads  :: IORef (Map.Map DID CidBytes)
  }

-- | Create a fresh, empty in-memory store.
newInMemoryStore :: IO InMemoryStore
newInMemoryStore = InMemoryStore <$> newIORef Map.empty <*> newIORef Map.empty

instance BlockStore InMemoryStore where
  getBlock s cid  = Map.lookup cid <$> readIORef (imsBlocks s)
  putBlock s cid bs = modifyIORef' (imsBlocks s) (Map.insert cid bs)

instance RepoStore InMemoryStore where
  getRepoHead s did = Map.lookup did <$> readIORef (imsHeads s)
  setRepoHead s did cid = modifyIORef' (imsHeads s) (Map.insert did cid)

-- ---------------------------------------------------------------------------
-- Per-actor store
-- ---------------------------------------------------------------------------

-- | A per-actor in-memory store scoped to a single DID.
--
-- Instances of 'ActorStorage' are created by 'InMemoryBackend' via
-- 'openActorStore'.
data InMemoryActorStore = InMemoryActorStore
  { imasBlocks :: IORef (Map.Map CidBytes BS.ByteString)
  , imasHead   :: IORef (Maybe CidBytes)
  }

instance ActorStorage InMemoryActorStore where
  getBlock s cid    = Map.lookup cid <$> readIORef (imasBlocks s)
  putBlock s cid bs = modifyIORef' (imasBlocks s) (Map.insert cid bs)
  getRepoHead s     = readIORef (imasHead s)
  setRepoHead s cid = writeIORef (imasHead s) (Just cid)

-- ---------------------------------------------------------------------------
-- Backend (factory)
-- ---------------------------------------------------------------------------

-- | A global in-memory backend that manages one 'InMemoryActorStore' per DID.
--
-- Opening the same DID twice returns the same underlying store.
data InMemoryBackend = InMemoryBackend
  { imbActors :: IORef (Map.Map DID InMemoryActorStore)
  }

-- | Create a fresh, empty 'InMemoryBackend'.
newInMemoryBackend :: IO InMemoryBackend
newInMemoryBackend = InMemoryBackend <$> newIORef Map.empty

instance ActorStoreBackend InMemoryBackend where
  type ActorStorageOf InMemoryBackend = InMemoryActorStore

  openActorStore b did = do
    actors <- readIORef (imbActors b)
    case Map.lookup did actors of
      Just s  -> return (ActorStore did s)
      Nothing -> do
        s <- InMemoryActorStore <$> newIORef Map.empty <*> newIORef Nothing
        modifyIORef' (imbActors b) (Map.insert did s)
        return (ActorStore did s)

