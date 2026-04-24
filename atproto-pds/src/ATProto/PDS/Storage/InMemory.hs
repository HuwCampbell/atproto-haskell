{-# LANGUAGE TypeFamilies #-}
-- | In-memory storage backend.
--
-- Uses 'IORef'-wrapped 'Data.Map.Strict.Map's.  Suitable for tests and
-- short-lived processes where persistence is not required.
--
-- Create a backend with 'newInMemoryBackend', then open per-actor stores
-- with 'openActorStore':
--
-- @
-- backend <- newInMemoryBackend
-- store   <- openActorStore backend did
-- Right _commit <- initRepo store privKey
-- @
--
-- Opening the same DID twice returns the same underlying store, so blocks
-- and the head pointer are shared within a DID but isolated between DIDs.
module ATProto.PDS.Storage.InMemory
  ( -- * Per-actor backend
    InMemoryBackend
  , newInMemoryBackend
  , InMemoryActorStore
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.IORef

import ATProto.Car.Cid        (CidBytes)
import ATProto.Syntax.DID     (DID)
import ATProto.PDS.Storage    (BlockStore (..), RepoStore (..))
import ATProto.PDS.ActorStore (ActorStore (..), ActorStoreBackend (..))
import Control.Monad.IO.Class (MonadIO(..))

-- ---------------------------------------------------------------------------
-- Per-actor store
-- ---------------------------------------------------------------------------

-- | A per-actor in-memory store scoped to a single DID.
--
-- Implements both 'BlockStore' and 'RepoStore'.  Created automatically
-- by 'InMemoryBackend' via 'openActorStore'.
data InMemoryActorStore = InMemoryActorStore
  { imasBlocks :: IORef (Map.Map CidBytes BS.ByteString)
  , imasHead   :: IORef (Maybe CidBytes)
  }

instance BlockStore InMemoryActorStore where
  getBlock s cid    = liftIO $ Map.lookup cid <$> readIORef (imasBlocks s)
  putBlock s cid bs = liftIO $ modifyIORef' (imasBlocks s) (Map.insert cid bs)

-- | The 'RepoStore' instance has no DID parameter; this store is already
--   scoped to a single actor by 'InMemoryBackend'.
instance RepoStore InMemoryActorStore where
  getRepoHead s     = liftIO $ readIORef (imasHead s)
  setRepoHead s cid = liftIO $ writeIORef (imasHead s) (Just cid)

-- ---------------------------------------------------------------------------
-- Backend (factory)
-- ---------------------------------------------------------------------------

-- | A global in-memory backend that manages one 'InMemoryActorStore' per DID.
--
-- Opening the same DID twice returns the same underlying store.
newtype InMemoryBackend = InMemoryBackend
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

  closeActorStore _ _ = pure ()
