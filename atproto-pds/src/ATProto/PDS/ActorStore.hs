{-# LANGUAGE TypeFamilies #-}
-- | DID-scoped actor store abstraction.
--
-- This module provides:
--
-- * 'ActorStore' – a handle that bundles a DID with its per-actor storage
--   value (which implements both 'BlockStore' and 'RepoStore').
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
  ( -- * Actor store handle
    ActorStore (..)
    -- * Backend class
  , ActorStoreBackend (..)
    -- * Convenience combinator
  , withActorStore
  ) where

import ATProto.Syntax.DID  (DID)
import ATProto.PDS.Storage (BlockStore, RepoStore)

-- | A handle to per-actor storage, bundling the DID with its scoped store.
--
-- The wrapped store @s@ implements both 'BlockStore' and 'RepoStore'.
-- Because 'RepoStore' carries no DID parameter, the DID stored in
-- 'actorDid' is available for operations that need it (e.g. commit
-- signing) without being threaded through every storage call.
--
-- Obtain one via 'openActorStore'.  Pass it directly to repository
-- operations in "ATProto.PDS.Repo".
data ActorStore s = ActorStore
  { actorDid     :: !DID
    -- ^ The DID this store is scoped to.
  , actorStorage :: !s
    -- ^ The underlying storage value (implements 'BlockStore' and 'RepoStore').
  }

-- | A backend capable of opening a scoped store for a given DID.
--
-- The associated type 'ActorStorageOf' must implement both 'BlockStore'
-- and 'RepoStore'.  Implement this class for your storage technology:
-- the @openActorStore@ call is the right place to open a per-DID database
-- connection, create on-disk directories, or look up a per-DID in-memory map.
class ( BlockStore (ActorStorageOf b)
      , RepoStore  (ActorStorageOf b)
      ) => ActorStoreBackend b where
  -- | The concrete storage type produced by this backend.
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
