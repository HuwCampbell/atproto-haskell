# Revision history for atproto-haskell-pds

## 0.2.0.0 -- 2026-04-21

* Redesigned `RepoStore` to be DID-free: `getRepoHead` and `setRepoHead`
  no longer take a `DID` parameter.  The store is now implicitly scoped to
  a single actor, opened via `openActorStore`.
* Added `ATProto.PDS.ActorStore` module exposing:
  - `ActorStore` – bundles a DID with its per-actor storage value (which
    implements both `BlockStore` and `RepoStore`).
  - `ActorStoreBackend` – factory class for opening per-DID stores via
    `openActorStore`.  The superclass constraint ensures the associated
    storage type implements both `BlockStore` and `RepoStore`.
  - `withActorStore` – convenience combinator.
* Updated all repository operations in `ATProto.PDS.Repo` to accept
  `ActorStore s` with a `(BlockStore s, RepoStore s)` constraint instead of
  `(s, DID)`, matching upstream's actor-store pattern.
* `ATProto.PDS.Storage.InMemory` now exports `InMemoryBackend` /
  `newInMemoryBackend` and `InMemoryActorStore`.  `InMemoryActorStore`
  implements both `BlockStore` and `RepoStore` (no DID).
* `ATProto.PDS.Storage.FileSystem` now exports `FileBackend` /
  `newFileBackend` and `FileActorStore`.  `FileActorStore` implements both
  `BlockStore` and `RepoStore` (no DID).  Each actor's blocks are stored
  under `blocks/<sanitised-did>/` for physical isolation.
* Updated `ATProto.PDS` re-export module to include the new `ActorStore`
  module and backends.
* Updated Haddock comments throughout to reflect the new design.

## 0.1.0.0 -- 2026-03-26

* Initial release.
* Storage type classes (`BlockStore`, `RepoStore`).
* In-memory and file-system storage backends.
* Repository operations: init, create/get/list/delete records.
* Commit creation and signing (AT Protocol v3 commits).
