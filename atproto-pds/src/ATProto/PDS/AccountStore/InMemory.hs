-- | In-memory account store backend.
--
-- Stores all account data in 'IORef'-wrapped 'Map's.  Suitable for tests
-- and short-lived processes where persistence across restarts is not required.
--
-- Create a store with 'newInMemoryAccountStore':
--
-- @
-- store <- newInMemoryAccountStore
-- createAccount store account signingKey
-- mAcc <- getAccount store (accountDid account)
-- @
module ATProto.PDS.AccountStore.InMemory
  ( InMemoryAccountStore
  , newInMemoryAccountStore
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import qualified Data.Map.Strict        as Map

import           Web.ClientSession      (Key, randomKey)

import           ATProto.Crypto.Types   (PrivKey)
import           ATProto.Syntax.DID     (DID)
import           ATProto.PDS.AccountStore

-- ---------------------------------------------------------------------------
-- Store state
-- ---------------------------------------------------------------------------

-- | All mutable state for the in-memory account store.
data InMemoryAccountStore = InMemoryAccountStore
  { imasAccounts       :: IORef (Map.Map DID Account)
  , imasSigningKeys    :: IORef (Map.Map DID PrivKey)
  , imasPasswordHashes :: IORef (Map.Map DID PasswordHash)
  , imasPlcRotKeys     :: IORef (Map.Map DID PrivKey)
  , imasSessionKey     :: Key
    -- ^ Symmetric key for producing and verifying clientsession tokens.
    -- Generated fresh at construction; tokens are invalidated on restart.
  }

-- | Create a fresh, empty 'InMemoryAccountStore'.
--
-- Generates a new random session key.  Session tokens produced by this
-- store are invalidated when the store is garbage-collected.
newInMemoryAccountStore :: IO InMemoryAccountStore
newInMemoryAccountStore = do
  (_rawKey, key) <- randomKey
  InMemoryAccountStore
    <$> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> pure key

-- ---------------------------------------------------------------------------
-- AccountStore instance
-- ---------------------------------------------------------------------------

instance AccountStore InMemoryAccountStore where
  createAccount s acc key = liftIO $ do
    modifyIORef' (imasAccounts    s) (Map.insert (accountDid acc) acc)
    modifyIORef' (imasSigningKeys s) (Map.insert (accountDid acc) key)

  getAccount s did = liftIO $
    Map.lookup did <$> readIORef (imasAccounts s)

  updateAccount s acc = liftIO $
    modifyIORef' (imasAccounts s) (Map.insert (accountDid acc) acc)

  deleteAccount s did = liftIO $ do
    modifyIORef' (imasAccounts       s) (Map.delete did)
    modifyIORef' (imasSigningKeys    s) (Map.delete did)
    modifyIORef' (imasPasswordHashes s) (Map.delete did)
    modifyIORef' (imasPlcRotKeys     s) (Map.delete did)

  storePassword s did h = liftIO $
    modifyIORef' (imasPasswordHashes s) (Map.insert did h)

  getPasswordHash s did = liftIO $
    Map.lookup did <$> readIORef (imasPasswordHashes s)

  getSigningKey s did = liftIO $
    Map.lookup did <$> readIORef (imasSigningKeys s)

  -- | Return the session key generated at construction time.
  getSessionKey s = return (imasSessionKey s)

  storePlcRotationKey s did key = liftIO $
    modifyIORef' (imasPlcRotKeys s) (Map.insert did key)

  getPlcRotationKey s did = liftIO $
    Map.lookup did <$> readIORef (imasPlcRotKeys s)
