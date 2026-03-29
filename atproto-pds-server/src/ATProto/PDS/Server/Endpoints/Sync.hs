-- | @com.atproto.sync.*@ endpoint handlers.
module ATProto.PDS.Server.Endpoints.Sync
  ( handleGetBlob
  , handleSyncGetRecord
  , handleGetRepo
  , handleGetRepoStatus
  , handleListBlobs
  , handleListRepos
  , handleSubscribeRepos
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import ATProto.Car.Cid               (cidToText, textToCidBytes)
import ATProto.PDS.Storage           (BlockStore (..), RepoStore (..),
                                      BlobStore (..), AccountStore (..),
                                      AccountInfo (..))
import ATProto.PDS.Repo              (exportRepoCar, getRecord)
import ATProto.Repo.Sync             (GetRepoStatusResponse (..),
                                      getRepoStatusResponseCodec,
                                      ListBlobsResponse (..),
                                      listBlobsResponseCodec,
                                      ListReposResponse (..),
                                      listReposResponseCodec,
                                      RepoInfo (..))
import ATProto.Syntax.DID            (unDID, parseDID)
import ATProto.XRPC.Server           (XrpcServerRequest (..), XrpcHandlerResult (..),
                                      runHandler, requireParam,
                                      respondCodec, respondRaw, throwXrpc)
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- com.atproto.sync.getBlob
-- ---------------------------------------------------------------------------

handleGetBlob
  :: BlobStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetBlob req = runHandler $ do
  cidText <- requireParam "cid" req
  cid <- case textToCidBytes cidText of
    Left _  -> throwXrpc "InvalidRequest" "Invalid CID"
    Right c -> return c
  store <- asks envStore
  mBlob <- liftIO $ getBlob store cid
  case mBlob of
    Nothing -> throwXrpc "BlobNotFound" "Blob not found"
    Just blobBytes -> respondRaw (BL.fromStrict blobBytes)

-- ---------------------------------------------------------------------------
-- com.atproto.sync.getRecord
-- ---------------------------------------------------------------------------

handleSyncGetRecord
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleSyncGetRecord req = runHandler $ do
  didText    <- requireParam "did" req
  collection <- requireParam "collection" req
  rkey       <- requireParam "rkey" req
  did <- case parseDID didText of
    Left _  -> throwXrpc "InvalidRequest" "Invalid DID"
    Right d -> return d
  store <- asks envStore
  mHead <- liftIO $ getRepoHead store did
  headCid <- case mHead of
    Nothing -> throwXrpc "RepoNotFound" "Repository not found"
    Just c  -> return c
  res <- liftIO $ getRecord store headCid collection rkey
  case res of
    Left _err     -> throwXrpc "InternalError" "Failed to read record"
    Right Nothing -> throwXrpc "RecordNotFound" "Record not found"
    Right (Just recordBytes) -> respondRaw (BL.fromStrict recordBytes)

-- ---------------------------------------------------------------------------
-- com.atproto.sync.getRepo
-- ---------------------------------------------------------------------------

handleGetRepo
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetRepo req = runHandler $ do
  didText <- requireParam "did" req
  did <- case parseDID didText of
    Left _  -> throwXrpc "InvalidRequest" "Invalid DID"
    Right d -> return d
  store <- asks envStore
  res <- liftIO $ exportRepoCar store did
  case res of
    Left _err     -> throwXrpc "RepoNotFound" "Repository not found"
    Right carBytes -> respondRaw (BL.fromStrict carBytes)

-- ---------------------------------------------------------------------------
-- com.atproto.sync.getRepoStatus
-- ---------------------------------------------------------------------------

handleGetRepoStatus
  :: (BlockStore s, RepoStore s, AccountStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetRepoStatus req = runHandler $ do
  didText <- requireParam "did" req
  did <- case parseDID didText of
    Left _  -> throwXrpc "InvalidRequest" "Invalid DID"
    Right d -> return d
  store <- asks envStore
  mHead <- liftIO $ getRepoHead store did
  headCid <- case mHead of
    Nothing -> throwXrpc "RepoNotFound" "Repository not found"
    Just c  -> return c
  mAcct <- liftIO $ getAccount store didText
  let active = maybe True aiActive mAcct
      status = if active then Just "active" else Just "deactivated"
  respondCodec getRepoStatusResponseCodec $
    GetRepoStatusResponse didText active status (Just (cidToText headCid))

-- ---------------------------------------------------------------------------
-- com.atproto.sync.listBlobs
-- ---------------------------------------------------------------------------

handleListBlobs
  :: BlobStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleListBlobs _req = runHandler $ do
  store <- asks envStore
  cids <- liftIO $ listBlobs store
  respondCodec listBlobsResponseCodec $
    ListBlobsResponse (map cidToText cids) Nothing

-- ---------------------------------------------------------------------------
-- com.atproto.sync.listRepos
-- ---------------------------------------------------------------------------

handleListRepos
  :: AccountStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleListRepos _req = runHandler $ do
  store <- asks envStore
  accts <- liftIO $ listAccounts store
  let repos = map (\ai ->
        let active = aiActive ai
            status = if active then Just "active" else Just "deactivated"
        in RepoInfo (unDID (aiDid ai)) active status
        ) accts
  respondCodec listReposResponseCodec $
    ListReposResponse repos Nothing

-- ---------------------------------------------------------------------------
-- com.atproto.sync.subscribeRepos  (stub)
-- ---------------------------------------------------------------------------

handleSubscribeRepos
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleSubscribeRepos _req =
  return $ XrpcHandlerError "MethodNotImplemented"
    (Just "subscribeRepos requires WebSocket transport (not yet supported)")
