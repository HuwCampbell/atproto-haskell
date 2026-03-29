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

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import           Data.IORef

import ATProto.Car.Cid        (CidBytes)
import ATProto.Syntax.DID     (DID)
import ATProto.PDS.Storage    (BlockStore (..), RepoStore (..),
                               BlobStore (..), PreferenceStore (..),
                               AccountStore (..), AccountInfo (..))

-- | An in-memory store backed by 'IORef' 'Map's.
data InMemoryStore = InMemoryStore
  { imsBlocks      :: IORef (Map.Map CidBytes BS.ByteString)
  , imsHeads       :: IORef (Map.Map DID CidBytes)
  , imsBlobs       :: IORef (Map.Map CidBytes BS.ByteString)
  , imsPreferences :: IORef (Map.Map T.Text BL.ByteString)
  , imsAccounts    :: IORef (Map.Map T.Text AccountInfo)
  , imsHandles     :: IORef (Map.Map T.Text DID)
  }

-- | Create a fresh, empty in-memory store.
newInMemoryStore :: IO InMemoryStore
newInMemoryStore =
  InMemoryStore
    <$> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty

instance BlockStore InMemoryStore where
  getBlock s cid  = Map.lookup cid <$> readIORef (imsBlocks s)
  putBlock s cid bs = modifyIORef' (imsBlocks s) (Map.insert cid bs)

instance RepoStore InMemoryStore where
  getRepoHead s did = Map.lookup did <$> readIORef (imsHeads s)
  setRepoHead s did cid = modifyIORef' (imsHeads s) (Map.insert did cid)

instance BlobStore InMemoryStore where
  getBlob s cid = Map.lookup cid <$> readIORef (imsBlobs s)
  putBlob s cid bs = modifyIORef' (imsBlobs s) (Map.insert cid bs)
  listBlobs s = Map.keys <$> readIORef (imsBlobs s)

instance PreferenceStore InMemoryStore where
  getPreferences s did = Map.lookup did <$> readIORef (imsPreferences s)
  putPreferences s did bs = modifyIORef' (imsPreferences s) (Map.insert did bs)

instance AccountStore InMemoryStore where
  getAccount s did = Map.lookup did <$> readIORef (imsAccounts s)
  putAccount s did ai = modifyIORef' (imsAccounts s) (Map.insert did ai)
  modifyAccount s did f = modifyIORef' (imsAccounts s) (Map.adjust f did)
  lookupByHandle s handle = Map.lookup handle <$> readIORef (imsHandles s)
  registerHandle s handle did = modifyIORef' (imsHandles s) (Map.insert handle did)
  listAccounts s = Map.elems <$> readIORef (imsAccounts s)
