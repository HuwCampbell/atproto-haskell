{-# LANGUAGE TypeFamilies #-}
-- | File-system storage backend.
--
-- Blocks are stored as individual files named by their base32-encoded CID.
-- The repository head pointer is a plain text file containing the CID.
-- Each actor's data lives in its own subdirectory, providing physical
-- isolation between DIDs.
--
-- Directory layout under the base directory:
--
-- @
-- basedir/
--   blocks/
--     \<sanitised-did\>/    ← one directory per actor
--       \<cid\>             ← one file per block
--   heads/
--     \<sanitised-did\>     ← one file per actor (contains the head CID)
-- @
--
-- Create a backend with 'newFileBackend', then open per-actor stores with
-- 'openActorStore':
--
-- @
-- backend <- newFileBackend "\/tmp\/my-pds"
-- store   <- openActorStore backend did
-- Right _commit <- initRepo store privKey
-- @
module ATProto.PDS.Storage.FileSystem
  ( -- * Per-actor backend
    FileBackend
  , newFileBackend
  , FileActorStore
  ) where

import           Control.Monad   (when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text       as T

import qualified Streaming.ByteString as Streaming
import           System.Directory  (createDirectoryIfMissing, doesFileExist, renameFile, removeFile)
import           System.FilePath   ((</>))
import           System.Random     (newStdGen, randomRs)

import           ATProto.Car.Cid     (cidToText, textToCidBytes)
import           ATProto.Syntax.DID  (DID, unDID)
import           ATProto.PDS.Storage (BlockStore (..), RepoStore (..), BlobStore (..))
import           ATProto.PDS.ActorStore (ActorStore (..), ActorStoreBackend (..))

-- ---------------------------------------------------------------------------
-- Per-actor store
-- ---------------------------------------------------------------------------

-- | A per-actor file-system store scoped to a single DID.
--
-- Blocks are stored under 'fasBlockDir' and the head pointer lives at
-- 'fasHeadFile'.  Implements both 'BlockStore' and 'RepoStore'.
data FileActorStore = FileActorStore
  { fasBlockDir :: FilePath
    -- ^ Directory for this actor's block files.
  , fasTempDir :: FilePath
    -- ^ Directory for this actor's block files.
  , fasBlobsDir :: FilePath
    -- ^ Directory for this actor's block files.
  , fasHeadFile :: FilePath
    -- ^ Path to this actor's head pointer file.
  }

instance BlockStore FileActorStore where
  getBlock s cid = liftIO $ do
    let path = fasBlockDir s </> T.unpack (cidToText cid)
    exists <- doesFileExist path
    if exists
      then Just <$> BS.readFile path
      else return Nothing

  putBlock s cid bs =
    liftIO $
      BS.writeFile (fasBlockDir s </> T.unpack (cidToText cid)) bs

  deleteBlock s cid =
    liftIO $
      removeFile (fasBlockDir s </> T.unpack (cidToText cid))

-- | The 'RepoStore' instance has no DID parameter; this store is already
--   scoped to a single actor by 'FileBackend'.
instance RepoStore FileActorStore where
  getRepoHead s = liftIO $ do
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
    liftIO $
      writeFile (fasHeadFile s) (T.unpack (cidToText cid))


instance BlobStore FileActorStore where
  type TempKey FileActorStore = FilePath

  putTemp store stream = do
    stdGen      <- liftIO newStdGen
    let tempKey  = take 12 (randomRs ('a', 'z') stdGen)
    res         <- Streaming.writeFile (fasTempDir store </> tempKey) stream
    pure (tempKey, res)

  makePermanent store tempKey cid = liftIO $ do
    exists <- doesFileExist (fasTempDir store </> tempKey)
    when exists $
      renameFile (fasTempDir store </> tempKey)
                 (fasBlobsDir store </> T.unpack (cidToText cid))

    return exists

  streamBytes store cid = do
    let path = fasBlobsDir store </> T.unpack (cidToText cid)
    Streaming.readFile path

  delete store cid = liftIO $ do
    let path = fasBlobsDir store </> T.unpack (cidToText cid)
    exists <- doesFileExist path
    when exists $
      removeFile path

    return exists

-- ---------------------------------------------------------------------------
-- Backend (factory)
-- ---------------------------------------------------------------------------

-- | A file-system backend rooted at a given directory.
--
-- Each actor's blocks are stored under @\<basedir\>\/blocks\/\<sanitised-did\>\/@
-- and its head pointer at @\<basedir\>\/heads\/\<sanitised-did\>@.
newtype FileBackend = FileBackend
  { fbBaseDir :: FilePath
    -- ^ Root directory for the backend.
  }

-- | Create a 'FileBackend' rooted at @basedir@.
--
-- Creates @basedir\/blocks\/@ and @basedir\/heads\/@ if they do not
-- already exist.
newFileBackend :: FilePath -> IO FileBackend
newFileBackend basedir = do
  createDirectoryIfMissing True basedir
  return (FileBackend basedir)

instance ActorStoreBackend FileBackend where
  type ActorStorageOf FileBackend = FileActorStore

  openActorStore b did = do
    let bd = fbBaseDir b </> sanitiseDID did </> "blocks"
        td = fbBaseDir b </> sanitiseDID did </> "temps"
        bs = fbBaseDir b </> sanitiseDID did </> "blobs"
        hf = fbBaseDir b </> sanitiseDID did </> "heads"
    createDirectoryIfMissing True bd
    createDirectoryIfMissing True td
    createDirectoryIfMissing True bs
    return (ActorStore did (FileActorStore bd td bs hf))

  closeActorStore _ _ = pure ()

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Produce a file-system–safe name from a DID.
--
-- Replaces @:@ with @_@ so the string is safe on all platforms.
sanitiseDID :: DID -> FilePath
sanitiseDID = T.unpack . T.map (\c -> if c == ':' then '_' else c) . unDID
