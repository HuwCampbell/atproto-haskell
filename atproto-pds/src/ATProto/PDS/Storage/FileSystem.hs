{-# LANGUAGE TypeFamilies #-}
-- | File-system storage backend.
--
-- The module exposes two levels of API:
--
-- * 'FileStore' / 'newFileStore' — a global file-system store implementing
--   the legacy 'BlockStore' and 'RepoStore' type classes (kept for
--   backward compatibility).  Blocks live in a flat @blocks\/@ directory
--   shared across all DIDs.
--
-- * 'FileBackend' / 'newFileBackend' — a factory that implements
--   'ActorStoreBackend', producing per-DID 'FileActorStore' values that
--   implement 'ActorStorage'.  Blocks are stored under
--   @blocks\/\<sanitised-did\>\/@ so each actor's data is physically
--   isolated.  This is the preferred API for use with "ATProto.PDS.Repo".
--
-- @
-- backend <- newFileBackend "\/tmp\/my-pds"
-- store   <- openActorStore backend did
-- Right _commit <- initRepo store privKey
-- @
module ATProto.PDS.Storage.FileSystem
  ( -- * Legacy global store
    FileStore
  , newFileStore
    -- * Per-actor backend
  , FileBackend
  , newFileBackend
  , FileActorStore
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import System.Directory  (createDirectoryIfMissing, doesFileExist)
import System.FilePath   ((</>))

import ATProto.Car.Cid     (CidBytes, cidToText, textToCidBytes)
import ATProto.Syntax.DID  (DID, unDID)
import ATProto.PDS.Storage (BlockStore (..), RepoStore (..))
import ATProto.PDS.ActorStore (ActorStorage (..), ActorStore (..), ActorStoreBackend (..))

-- ---------------------------------------------------------------------------
-- Legacy global store
-- ---------------------------------------------------------------------------

-- | A file-system–backed store rooted at a given directory.
--
-- This type is kept for backward compatibility.  Prefer 'FileBackend'
-- for new code.
data FileStore = FileStore
  { fsBlockDir :: FilePath
    -- ^ Directory for block files (shared across all DIDs).
  , fsHeadDir  :: FilePath
    -- ^ Directory for head pointer files (one file per DID).
  }

-- | Create a 'FileStore' rooted at @basedir@.
--
-- Creates @basedir\/blocks\/@ and @basedir\/heads\/@ if they do not
-- already exist.
newFileStore :: FilePath -> IO FileStore
newFileStore basedir = do
  let bd = basedir </> "blocks"
      hd = basedir </> "heads"
  createDirectoryIfMissing True bd
  createDirectoryIfMissing True hd
  return (FileStore bd hd)

-- ---------------------------------------------------------------------------
-- BlockStore instance
-- ---------------------------------------------------------------------------

instance BlockStore FileStore where
  getBlock s cid = do
    let path = blockPath s cid
    exists <- doesFileExist path
    if exists
      then Just <$> BS.readFile path
      else return Nothing

  putBlock s cid = BS.writeFile (blockPath s cid)

blockPath :: FileStore -> CidBytes -> FilePath
blockPath s cid = fsBlockDir s </> T.unpack (cidToText cid)

-- ---------------------------------------------------------------------------
-- RepoStore instance
-- ---------------------------------------------------------------------------

instance RepoStore FileStore where
  getRepoHead s did = do
    let path = headPath s did
    exists <- doesFileExist path
    if exists
      then do
        cidText <- T.pack <$> readFile path
        case textToCidBytes cidText of
          Right cid -> return (Just cid)
          Left _    -> return Nothing
      else return Nothing

  setRepoHead s did cid =
    writeFile (headPath s did) (T.unpack (cidToText cid))

headPath :: FileStore -> DID -> FilePath
headPath s did = fsHeadDir s </> sanitiseDID did

-- ---------------------------------------------------------------------------
-- Per-actor store
-- ---------------------------------------------------------------------------

-- | A per-actor file-system store scoped to a single DID.
--
-- Blocks are stored under @\<basedir\>\/blocks\/\<sanitised-did\>\/@ and
-- the head pointer lives at @\<basedir\>\/heads\/\<sanitised-did\>@.
data FileActorStore = FileActorStore
  { fasBlockDir :: FilePath
    -- ^ Directory for this actor's block files.
  , fasHeadFile :: FilePath
    -- ^ Path to this actor's head pointer file.
  }

instance ActorStorage FileActorStore where
  getBlock s cid = do
    let path = fasBlockDir s </> T.unpack (cidToText cid)
    exists <- doesFileExist path
    if exists
      then Just <$> BS.readFile path
      else return Nothing

  putBlock s cid bs =
    BS.writeFile (fasBlockDir s </> T.unpack (cidToText cid)) bs

  getRepoHead s = do
    let path = fasHeadFile s
    exists <- doesFileExist path
    if exists
      then do
        cidText <- T.pack <$> readFile path
        case textToCidBytes cidText of
          Right cid -> return (Just cid)
          Left _    -> return Nothing
      else return Nothing

  setRepoHead s cid =
    writeFile (fasHeadFile s) (T.unpack (cidToText cid))

-- ---------------------------------------------------------------------------
-- Backend (factory)
-- ---------------------------------------------------------------------------

-- | A file-system backend rooted at a given directory.
--
-- Each actor's blocks are stored under @\<basedir\>\/blocks\/\<sanitised-did\>\/@
-- and its head pointer at @\<basedir\>\/heads\/\<sanitised-did\>@.
data FileBackend = FileBackend
  { fbBaseDir :: FilePath
    -- ^ Root directory for the backend.
  }

-- | Create a 'FileBackend' rooted at @basedir@.
--
-- Creates @basedir\/blocks\/@ and @basedir\/heads\/@ if they do not
-- already exist.
newFileBackend :: FilePath -> IO FileBackend
newFileBackend basedir = do
  createDirectoryIfMissing True (basedir </> "blocks")
  createDirectoryIfMissing True (basedir </> "heads")
  return (FileBackend basedir)

instance ActorStoreBackend FileBackend where
  type ActorStorageOf FileBackend = FileActorStore

  openActorStore b did = do
    let bd = fbBaseDir b </> "blocks" </> sanitiseDID did
        hf = fbBaseDir b </> "heads"  </> sanitiseDID did
    createDirectoryIfMissing True bd
    return (ActorStore did (FileActorStore bd hf))

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Produce a file-system–safe name from a DID.
--
-- Replaces @:@ with @_@ so the string is safe on all platforms.
sanitiseDID :: DID -> FilePath
sanitiseDID = T.unpack . T.map (\c -> if c == ':' then '_' else c) . unDID
