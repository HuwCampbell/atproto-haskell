-- | AT Protocol Personal Data Server – top-level re-export module.
--
-- Import this module for convenient access to the PDS API:
--
-- @
-- import ATProto.PDS
-- @
--
-- Or import individual sub-modules for finer-grained control:
--
-- @
-- import ATProto.PDS.Storage      -- legacy BlockStore / RepoStore
-- import ATProto.PDS.ActorStore   -- ActorStorage, ActorStore, ActorStoreBackend
-- import ATProto.PDS.Repo
-- @
--
-- Note: 'BlockStore' / 'RepoStore' and 'ActorStorage' both define methods
-- named @getBlock@, @putBlock@, @getRepoHead@, and @setRepoHead@.  To avoid
-- ambiguity this module exports the legacy type classes without their methods;
-- import "ATProto.PDS.Storage" directly if you need those methods.
module ATProto.PDS
  ( -- * Legacy storage type classes (methods via "ATProto.PDS.Storage")
    BlockStore
  , RepoStore
    -- * Actor store abstraction and DID-scoped storage
  , module ATProto.PDS.ActorStore
    -- * Storage backends
  , module ATProto.PDS.Storage.InMemory
  , module ATProto.PDS.Storage.FileSystem
    -- * Commit encoding
  , module ATProto.PDS.Commit
    -- * Repository operations
  , module ATProto.PDS.Repo
  ) where

import ATProto.PDS.Storage          (BlockStore, RepoStore)
import ATProto.PDS.ActorStore
import ATProto.PDS.Storage.InMemory
import ATProto.PDS.Storage.FileSystem
import ATProto.PDS.Commit
import ATProto.PDS.Repo
