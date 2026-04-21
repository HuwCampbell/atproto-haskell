{-# LANGUAGE TypeFamilies #-}
-- | DID-scoped actor store abstraction.
--
-- This module provides:
--
-- * 'ActorStorage' – a DID-free type class combining block and head operations
--   for a single actor's repository.  It is the preferred interface for all
--   repository operations in "ATProto.PDS.Repo".
--
-- * 'ActorStore' – a handle that bundles a DID with its per-actor storage value.
--
-- * 'ActorStoreBackend' – a factory class whose 'openActorStore' call opens
--   (or reuses) a per-DID store.  Implement this for your storage technology.
--
-- The actor-store pattern mirrors the upstream AT Protocol PDS design: a
-- backend is opened once per actor, and all subsequent operations are
-- implicitly scoped to that actor.  This makes it structurally impossible to
-- write blocks into the wrong actor's store, and is a natural fit for per-DID
-- backends such as SQLite.
--
-- @
-- backend <- newInMemoryBackend
-- let Right did = parseDID "did:plc:example"
-- store <- openActorStore backend did
-- Right _commit <- initRepo store privKey
-- @
module ATProto.PDS.ActorStore
  ( -- * DID-scoped storage interface
    ActorStorage (..)
    -- * Actor store handle
  , ActorStore (..)
    -- * Backend class
  , ActorStoreBackend (..)
    -- * Convenience combinator
  , withActorStore
  ) where

import qualified Data.ByteString as BS

import ATProto.Car.Cid    (CidBytes)
import ATProto.Syntax.DID (DID)

-- | DID-scoped storage for a single actor's repository.
--
-- All block and head operations implicitly target the actor this store
-- was opened for.  Use 'openActorStore' to obtain a value of this class
-- for a specific DID.
--
-- This is the preferred interface for repository operations: it makes
-- it structurally impossible to write blocks into the wrong actor's
-- store, and is a natural fit for per-DID backends such as SQLite.
class ActorStorage s where
  -- | Retrieve a block by its CID, or 'Nothing' if absent.
  getBlock    :: s -> CidBytes -> IO (Maybe BS.ByteString)
  -- | Store a block under its CID.  Storing the same CID twice is
  --   idempotent.
  putBlock    :: s -> CidBytes -> BS.ByteString -> IO ()
  -- | Get the head commit CID for this actor's repository, or
  --   'Nothing' if the repository has not been initialised.
  getRepoHead :: s -> IO (Maybe CidBytes)
  -- | Set the head commit CID for this actor's repository.
  setRepoHead :: s -> CidBytes -> IO ()

-- | A handle to per-actor storage, bundling the DID with its scoped store.
--
-- Obtain one via 'openActorStore'.  Pass it directly to repository
-- operations in "ATProto.PDS.Repo".
data ActorStore s = ActorStore
  { actorDid     :: !DID
    -- ^ The DID this store is scoped to.
  , actorStorage :: !s
    -- ^ The underlying 'ActorStorage' value.
  }

-- | A backend capable of opening a scoped store for a given DID.
--
-- Implement this for your storage technology.  The @openActorStore@
-- call is the right place to open a per-DID database connection,
-- create on-disk directories, or look up a per-DID in-memory map.
class ActorStorage (ActorStorageOf b) => ActorStoreBackend b where
  -- | The concrete 'ActorStorage' type produced by this backend.
  type ActorStorageOf b :: *

  -- | Open (or reuse) the scoped store for @did@.
  --
  -- For a SQLite backend this would open @\<did\>.db@.  For the
  -- in-memory backend it looks up or creates the per-DID entry.
  openActorStore :: b -> DID -> IO (ActorStore (ActorStorageOf b))

-- | Open an actor store, run an action with it, and return the result.
--
-- This is a convenience wrapper around 'openActorStore'.
withActorStore
  :: ActorStoreBackend b
  => b
  -> DID
  -> (ActorStore (ActorStorageOf b) -> IO a)
  -> IO a
withActorStore b did f = openActorStore b did >>= f

