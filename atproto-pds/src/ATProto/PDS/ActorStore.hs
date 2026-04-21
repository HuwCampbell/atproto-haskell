{-# LANGUAGE TypeFamilies #-}
-- | DID-scoped actor store abstraction.
--
-- This module provides:
--
-- * 'ActorStore' – a handle that bundles a DID with its per-actor storage
--   value (which implements both 'BlockStore' and 'RepoStore').
--
-- * 'ActorStoreBackend' – a factory class whose 'openActorStore' and
--   'closeActorStore' calls manage the lifetime of a per-DID store.
--   Implement this for your storage technology.
--
-- The actor-store pattern mirrors the upstream AT Protocol PDS design: a
-- backend is opened once per actor, all operations execute, and then the
-- store is closed — even if an exception is raised.  This makes it
-- structurally impossible to write blocks into the wrong actor's store,
-- and is a natural fit for per-DID backends such as SQLite (where
-- 'openActorStore' opens the database and 'closeActorStore' closes it).
--
-- Use 'withActorStore' to get the bracket behaviour automatically:
--
-- @
-- backend <- newInMemoryBackend
-- let Right did = parseDID "did:plc:example"
-- withActorStore backend did $ \\store -> do
--   Right _commit <- initRepo store privKey
--   ...
-- @
module ATProto.PDS.ActorStore
  ( -- * Actor store handle
    ActorStore (..)
    -- * Backend class
  , ActorStoreBackend (..)
    -- * Bracket combinator
  , withActorStore
  ) where

import Control.Exception   (bracket)
import ATProto.Syntax.DID  (DID)
import ATProto.PDS.Storage (BlockStore, RepoStore)

-- | A handle to per-actor storage, bundling the DID with its scoped store.
--
-- The wrapped store @s@ implements both 'BlockStore' and 'RepoStore'.
-- Because 'RepoStore' carries no DID parameter, the DID stored in
-- 'actorDid' is available for operations that need it (e.g. commit
-- signing) without being threaded through every storage call.
--
-- Obtain one via 'withActorStore' to ensure the store is always closed,
-- or via 'openActorStore' \/ 'closeActorStore' directly if you need
-- finer-grained control.
data ActorStore s = ActorStore
  { actorDid     :: !DID
    -- ^ The DID this store is scoped to.
  , actorStorage :: !s
    -- ^ The underlying storage value (implements 'BlockStore' and 'RepoStore').
  }

-- | A backend capable of opening and closing a scoped store for a given DID.
--
-- The associated type 'ActorStorageOf' must implement both 'BlockStore'
-- and 'RepoStore'.  Implement this class for your storage technology.
--
-- Minimal implementation: define 'openActorStore'.  The default
-- 'closeActorStore' is a no-op, suitable for backends (such as the
-- in-memory and file-system ones) that hold no closeable resources.
-- Override it for backends that do hold resources (e.g. SQLite), so that
-- 'withActorStore' can release them even when an exception is raised.
class ( BlockStore (ActorStorageOf b)
      , RepoStore  (ActorStorageOf b)
      ) => ActorStoreBackend b where
  -- | The concrete storage type produced by this backend.
  type ActorStorageOf b :: *

  -- | Open the scoped store for @did@.
  --
  -- For a SQLite backend this would open (or create) @\<did\>.db@.
  -- For the in-memory backend it looks up or creates the per-DID entry.
  --
  -- Prefer 'withActorStore' over calling this directly so that
  -- 'closeActorStore' is guaranteed to run.
  openActorStore :: b -> DID -> IO (ActorStore (ActorStorageOf b))

  -- | Close the scoped store, releasing any resources it holds.
  --
  -- The default implementation is a no-op; override it for backends that
  -- hold closeable resources such as database connections or file handles.
  --
  -- This is called by 'withActorStore' in its cleanup phase, so it runs
  -- even when the action raises an exception.
  closeActorStore :: b -> ActorStore (ActorStorageOf b) -> IO ()
  closeActorStore _ _ = return ()

-- | Open a scoped store for @did@, run @action@, close the store, and
-- return the result.
--
-- 'closeActorStore' is called in the cleanup phase of 'bracket', so the
-- store is always closed — even if @action@ raises an exception.  This
-- prevents resource leaks for backends that hold database connections or
-- other closeable handles.
withActorStore
  :: ActorStoreBackend b
  => b
  -> DID
  -> (ActorStore (ActorStorageOf b) -> IO a)
  -> IO a
withActorStore b did =
  bracket (openActorStore b did) (closeActorStore b)
