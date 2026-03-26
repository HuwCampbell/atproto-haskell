-- | In-memory storage backend.
--
-- Uses 'IORef'-wrapped 'Data.Map.Strict.Map's.  Suitable for tests and
-- short-lived processes where persistence is not required.
--
-- @
-- store <- newInMemoryStore
-- Right commitCid <- initRepo store did privKey
-- @
module ATProto.PDS.Storage.InMemory
  ( InMemoryStore
  , newInMemoryStore
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.IORef

import ATProto.Car.Cid        (CidBytes)
import ATProto.Syntax.DID     (DID)
import ATProto.PDS.Storage    (BlockStore (..), RepoStore (..))

-- | An in-memory store backed by 'IORef' 'Map's.
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
