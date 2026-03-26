-- | File-system storage backend.
--
-- Blocks are stored as individual files in a @blocks/@ subdirectory,
-- named by their base32-encoded CID.  Repository heads are stored in
-- a @heads/@ subdirectory, keyed by a sanitised DID.
--
-- @
-- store <- newFileStore "\/tmp\/my-pds"
-- Right commitCid <- initRepo store did privKey
-- @
module ATProto.PDS.Storage.FileSystem
  ( FileStore
  , newFileStore
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import System.Directory  (createDirectoryIfMissing, doesFileExist)
import System.FilePath   ((</>))

import ATProto.Car.Cid     (CidBytes, cidToText, textToCidBytes)
import ATProto.Syntax.DID  (DID, unDID)
import ATProto.PDS.Storage (BlockStore (..), RepoStore (..))

-- | A file-system–backed store rooted at a given directory.
data FileStore = FileStore
  { fsBlockDir :: FilePath
    -- ^ Directory for block files.
  , fsHeadDir  :: FilePath
    -- ^ Directory for head pointer files.
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

-- | Produce a file-system–safe name from a DID.
--
-- Replaces @:@ with @_@ so the string is safe on all platforms.
sanitiseDID :: DID -> FilePath
sanitiseDID = T.unpack . T.map (\c -> if c == ':' then '_' else c) . unDID
