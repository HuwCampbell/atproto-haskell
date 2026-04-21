# Revision history for atproto-haskell-pds

## 0.2.0.0 -- 2026-04-21

* Introduced `ActorStorage` type class in `ATProto.PDS.Storage`: a
  DID-scoped storage interface with no DID in any method signature.
* Added `ATProto.PDS.ActorStore` module exposing `ActorStore` (bundles a
  DID with its per-actor storage handle) and `ActorStoreBackend` (factory
  class for opening per-DID stores via `openActorStore`).
* Updated all repository operations in `ATProto.PDS.Repo` to accept
  `ActorStore s` instead of `(s, DID)`, matching upstream's actor-store
  pattern and making cross-actor block writes structurally impossible.
* `ATProto.PDS.Storage.InMemory` now exports `InMemoryBackend` /
  `newInMemoryBackend` and `InMemoryActorStore`, which implement
  `ActorStoreBackend` and `ActorStorage` respectively.
  `InMemoryStore` / `newInMemoryStore` are retained for backward
  compatibility.
* `ATProto.PDS.Storage.FileSystem` now exports `FileBackend` /
  `newFileBackend` and `FileActorStore`, which implement
  `ActorStoreBackend` and `ActorStorage` respectively.  Each actor's
  blocks are stored under `blocks/<sanitised-did>/` for physical isolation.
  `FileStore` / `newFileStore` are retained for backward compatibility.
* Updated `ATProto.PDS` re-export module to include `ActorStore`,
  `ActorStoreBackend`, and the new backend types.
* Updated Haddock comments throughout to reflect the new design.

## 0.1.0.0 -- 2026-03-26

* Initial release.
* Storage type classes (`BlockStore`, `RepoStore`).
* In-memory and file-system storage backends.
* Repository operations: init, create/get/list/delete records.
* Commit creation and signing (AT Protocol v3 commits).
