-- | Repository operations for the PDS.
--
-- This module provides the high-level API for managing AT Protocol
-- repositories: initialisation, record creation\/retrieval\/listing\/deletion,
-- and batch writes.
--
-- All operations are generic over any storage backend that implements
-- 'BlockStore' and 'RepoStore'.
--
-- @
-- import ATProto.PDS.Storage.InMemory (newInMemoryStore)
-- import ATProto.PDS.Repo
-- import ATProto.Crypto (generateKeyPair, Curve(..))
-- import ATProto.Syntax  (parseDID)
--
-- main :: IO ()
-- main = do
--   store <- newInMemoryStore
--   (priv, _pub) <- generateKeyPair P256
--   let Right did = parseDID \"did:plc:example\"
--   Right _commit <- initRepo store did priv
--   Right _commit <- createRecord store did priv
--                      \"app.bsky.feed.post\" \"3jqfcqzm3fn2j\"
--                      \"{\\\"text\\\": \\\"hello\\\"}\"
-- @
module ATProto.PDS.Repo
  ( -- * Errors
    PdsError (..)
    -- * Initialisation
  , initRepo
    -- * Record operations
  , createRecord
  , getRecord
  , listRecords
  , deleteRecord
    -- * Batch writes
  , WriteOp (..)
  , applyWrites
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Codec.CBOR.Decoding  as D
import qualified Codec.CBOR.Read      as R

import ATProto.Car.Cid      (CidBytes, cidForDagCbor)
import ATProto.Car.BlockMap  (BlockMap)
import ATProto.Car.DagCbor   (decodeCidTag42, skipValue)
import ATProto.MST.Build     (buildMST)
import ATProto.MST.Diff      (mstDiff, WriteDescr (..))
import ATProto.MST.Encode    (encodeNode)
import ATProto.MST.Get       (get)
import ATProto.MST.Node      (NodeData (..), TreeEntry (..), decodeNode)
import ATProto.MST.Types     (MstError (..))
import ATProto.Crypto.Types  (PrivKey)
import ATProto.Syntax.DID    (DID, unDID)
import ATProto.Syntax.TID    (tidNow, unTID)
import ATProto.PDS.Storage   (BlockStore (..), RepoStore (..))
import ATProto.PDS.Commit    (createSignedCommit)

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

-- | Errors that can occur during PDS repository operations.
data PdsError
  = PdsRepoNotFound T.Text
    -- ^ No repository exists for the given DID.
  | PdsRepoAlreadyExists T.Text
    -- ^ A repository already exists for the given DID.
  | PdsBlockNotFound T.Text
    -- ^ A required block was not found in storage.
  | PdsMstError T.Text
    -- ^ An error occurred during MST operations.
  | PdsCommitDecodeError T.Text
    -- ^ The commit block could not be decoded.
  | PdsRecordExists T.Text
    -- ^ The record already exists (for create operations).
  | PdsRecordNotFound T.Text
    -- ^ The record does not exist (for delete operations).
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Repository initialisation
-- ---------------------------------------------------------------------------

-- | Initialise a new, empty repository for the given DID.
--
-- Creates an empty MST root node and a signed v3 commit, stores all
-- blocks, and sets the repository head.
--
-- Returns the CID of the initial commit, or 'PdsRepoAlreadyExists' if
-- the DID already has a repository.
initRepo
  :: (BlockStore s, RepoStore s)
  => s -> DID -> PrivKey -> IO (Either PdsError CidBytes)
initRepo store did key = do
  existing <- getRepoHead store did
  case existing of
    Just _  -> return (Left (PdsRepoAlreadyExists (unDID did)))
    Nothing -> do
      -- Build an empty MST (single node, no entries).
      let emptyNode  = NodeData Nothing []
          emptyBytes = encodeNode emptyNode
          emptyRoot  = cidForDagCbor emptyBytes
      putBlock store emptyRoot emptyBytes

      -- Create the signed commit.
      rev <- unTID <$> tidNow
      (commitCid, commitBytes) <- createSignedCommit (unDID did) key rev Nothing emptyRoot
      putBlock store commitCid commitBytes

      -- Set the head.
      setRepoHead store did commitCid
      return (Right commitCid)

-- ---------------------------------------------------------------------------
-- Record operations
-- ---------------------------------------------------------------------------

-- | Create a new record in the repository.
--
-- The record body is stored as a raw DAG-CBOR block.  A new MST is
-- built with the additional entry and a new signed commit is created.
--
-- Returns the CID of the new commit.
createRecord
  :: (BlockStore s, RepoStore s)
  => s
  -> DID
  -> PrivKey
  -> T.Text           -- ^ Collection NSID (e.g. @\"app.bsky.feed.post\"@)
  -> T.Text           -- ^ Record key
  -> BS.ByteString    -- ^ Record body (DAG-CBOR bytes)
  -> IO (Either PdsError CidBytes)
createRecord store did key collection rkey recordBytes =
  applyWrites store did key [Create collection rkey recordBytes]

-- | Retrieve a record from the repository.
--
-- Returns the raw record bytes if found, 'Nothing' if the key does not
-- exist in the collection, or a 'PdsError' on storage/decoding failures.
getRecord
  :: BlockStore s
  => s
  -> CidBytes         -- ^ Commit CID (head of the repo)
  -> T.Text           -- ^ Collection NSID
  -> T.Text           -- ^ Record key
  -> IO (Either PdsError (Maybe BS.ByteString))
getRecord store commitCid collection rkey = do
  r <- loadCommitData store commitCid
  case r of
    Left err -> return (Left err)
    Right (dataRoot, mstBlocks) -> do
      let mstKey = collection <> "/" <> rkey
      case get mstBlocks dataRoot mstKey of
        Left mstErr         -> return (Left (mapMstError mstErr))
        Right Nothing       -> return (Right Nothing)
        Right (Just recCid) -> do
          mBlock <- getBlock store recCid
          case mBlock of
            Nothing -> return (Left (PdsBlockNotFound (T.pack (show recCid))))
            Just bs -> return (Right (Just bs))

-- | List all records in a collection.
--
-- Returns @(recordKey, recordBytes)@ pairs sorted by record key.
listRecords
  :: BlockStore s
  => s
  -> CidBytes         -- ^ Commit CID (head of the repo)
  -> T.Text           -- ^ Collection NSID
  -> IO (Either PdsError [(T.Text, BS.ByteString)])
listRecords store commitCid collection = do
  r <- loadCommitData store commitCid
  case r of
    Left err -> return (Left err)
    Right (dataRoot, mstBlocks) -> do
      -- Extract all MST entries via diff from empty.
      case mstDiff mstBlocks Nothing dataRoot of
        Left mstErr -> return (Left (mapMstError mstErr))
        Right writes -> do
          let prefix = collection <> "/"
              matching = [ (T.drop (T.length prefix) (wdKey w), wdCid w)
                         | w <- writes
                         , prefix `T.isPrefixOf` wdKey w
                         ]
          records <- mapM (loadRecord store) matching
          return (sequence records)

-- | Delete a record from the repository.
--
-- Removes the record's MST entry and creates a new signed commit.
-- Returns the CID of the new commit.
deleteRecord
  :: (BlockStore s, RepoStore s)
  => s
  -> DID
  -> PrivKey
  -> T.Text           -- ^ Collection NSID
  -> T.Text           -- ^ Record key
  -> IO (Either PdsError CidBytes)
deleteRecord store did key collection rkey =
  applyWrites store did key [Delete collection rkey]

-- ---------------------------------------------------------------------------
-- Batch writes
-- ---------------------------------------------------------------------------

-- | A single write operation within a batch.
data WriteOp
  = Create T.Text T.Text BS.ByteString
    -- ^ @Create collection rkey recordBytes@
  | Update T.Text T.Text BS.ByteString
    -- ^ @Update collection rkey recordBytes@
  | Delete T.Text T.Text
    -- ^ @Delete collection rkey@
  deriving (Eq, Show)

-- | Apply a batch of write operations to the repository.
--
-- All operations are applied atomically: the MST is rebuilt and a
-- single new commit is signed.  Returns the CID of the new commit.
applyWrites
  :: (BlockStore s, RepoStore s)
  => s -> DID -> PrivKey -> [WriteOp]
  -> IO (Either PdsError CidBytes)
applyWrites store did key ops = do
  mHead <- getRepoHead store did
  case mHead of
    Nothing -> return (Left (PdsRepoNotFound (unDID did)))
    Just headCid -> do
      r <- loadCommitData store headCid
      case r of
        Left err -> return (Left err)
        Right (dataRoot, mstBlocks) -> do
          -- Get current entries from the MST.
          let currentEntries = case mstDiff mstBlocks Nothing dataRoot of
                Left _  -> []
                Right ws -> [ (wdKey w, wdCid w) | w <- ws ]

          -- Apply the write operations.
          result <- applyOps store currentEntries ops
          case result of
            Left err -> return (Left err)
            Right newEntries -> do
              -- Build new MST from the updated entry list.
              let (mNewRoot, newMstBlocks) = buildMST (List.sortBy (\a b -> compare (fst a) (fst b)) newEntries)

              -- Handle empty MST case.
              newDataRoot <- case mNewRoot of
                Just root -> do
                  root <$ Map.traverseWithKey (putBlock store) newMstBlocks
                Nothing -> do
                  -- Empty MST: create an explicit empty node.
                  let emptyNode  = NodeData Nothing []
                      emptyBytes = encodeNode emptyNode
                      emptyRoot  = cidForDagCbor emptyBytes
                  putBlock store emptyRoot emptyBytes
                  return emptyRoot

              -- Create and store the new commit.
              rev <- unTID <$> tidNow
              (commitCid, commitBytes) <-
                createSignedCommit (unDID did) key rev (Just headCid) newDataRoot
              putBlock store commitCid commitBytes
              setRepoHead store did commitCid
              return (Right commitCid)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Apply a list of write operations to the current entry list.
applyOps
  :: BlockStore s
  => s
  -> [(T.Text, CidBytes)]   -- ^ current entries
  -> [WriteOp]
  -> IO (Either PdsError [(T.Text, CidBytes)])
applyOps store = go
  where
    go entries [] = return (Right entries)
    go entries (op:rest) =
      case op of
        Create col rk recordBytes -> do
          let mstKey = col <> "/" <> rk
          case lookup mstKey entries of
            Just _  -> return (Left (PdsRecordExists mstKey))
            Nothing -> do
              let recCid = cidForDagCbor recordBytes
              putBlock store recCid recordBytes
              go (entries ++ [(mstKey, recCid)]) rest

        Update col rk recordBytes -> do
          let mstKey = col <> "/" <> rk
              withoutOld = filter (\(k, _) -> k /= mstKey) entries
              recCid = cidForDagCbor recordBytes
          putBlock store recCid recordBytes
          go (withoutOld ++ [(mstKey, recCid)]) rest

        Delete col rk -> do
          let mstKey = col <> "/" <> rk
          case lookup mstKey entries of
            Nothing -> return (Left (PdsRecordNotFound mstKey))
            Just _  ->
              go (filter (\(k, _) -> k /= mstKey) entries) rest

-- | Load the commit's data root CID and all MST blocks.
loadCommitData
  :: BlockStore s
  => s
  -> CidBytes
  -> IO (Either PdsError (CidBytes, BlockMap))
loadCommitData store commitCid = do
  mCommitBlock <- getBlock store commitCid
  case mCommitBlock of
    Nothing -> return (Left (PdsBlockNotFound (T.pack (show commitCid))))
    Just commitBytes ->
      case decodeCommitDataRoot commitBytes of
        Left err -> return (Left (PdsCommitDecodeError (T.pack err)))
        Right dataRoot -> do
          mstBlocks <- collectMstBlocks store dataRoot
          return (Right (dataRoot, mstBlocks))

-- | Decode just the @data@ field (MST root CID) from a commit block.
--
-- This is a lightweight decoder that extracts only what we need
-- without decoding the full commit structure.
decodeCommitDataRoot :: BS.ByteString -> Either String CidBytes
decodeCommitDataRoot bs = do
  let lazyBs = BL.fromStrict bs
  case R.deserialiseFromBytes dataRootDecoder lazyBs of
    Left  err    -> Left (show err)
    Right (_, c) -> Right c
  where
    dataRootDecoder :: D.Decoder s CidBytes
    dataRootDecoder = do
      n <- D.decodeMapLen
      findDataField n

    findDataField :: Int -> D.Decoder s CidBytes
    findDataField 0 = fail "commit missing 'data' field"
    findDataField n = do
      key <- D.decodeString
      if key == "data"
        then decodeCidTag42
        else skipValue >> findDataField (n - 1)

-- | Collect all MST node blocks reachable from a root CID.
--
-- Recursively traverses the MST, loading each node from storage and
-- adding it to the block map.
collectMstBlocks :: BlockStore s => s -> CidBytes -> IO BlockMap
collectMstBlocks store rootCid = go Map.empty [rootCid]
  where
    go bmap [] = return bmap
    go bmap (cid:rest)
      | Map.member cid bmap = go bmap rest
      | otherwise = do
          mBlock <- getBlock store cid
          case mBlock of
            Nothing -> go bmap rest
            Just raw ->
              case decodeNode raw of
                Left _ -> go (Map.insert cid raw bmap) rest
                Right nd ->
                  let bmap' = Map.insert cid raw bmap
                      children = nodeChildren nd
                  in go bmap' (children ++ rest)

    nodeChildren :: NodeData -> [CidBytes]
    nodeChildren nd =
      maybe [] (:[]) (nodeLeft nd) ++
      concatMap entryChildren (nodeEntries nd)

    entryChildren :: TreeEntry -> [CidBytes]
    entryChildren te = maybe [] (:[]) (teRightTree te)

-- | Load a record block by its CID.
loadRecord
  :: BlockStore s
  => s
  -> (T.Text, CidBytes)
  -> IO (Either PdsError (T.Text, BS.ByteString))
loadRecord store (rkey, cid) = do
  mBlock <- getBlock store cid
  case mBlock of
    Nothing -> return (Left (PdsBlockNotFound (T.pack (show cid))))
    Just bs -> return (Right (rkey, bs))

-- | Map an MST error to a PDS error.
mapMstError :: MstError -> PdsError
mapMstError (MstNodeNotFound t) = PdsMstError ("node not found: " <> t)
mapMstError (MstDecodeError t)  = PdsMstError ("decode error: " <> t)
