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
import           Data.IORef                 (readIORef)
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import ATProto.Car.Cid               (cidToText, textToCidBytes)
import ATProto.PDS.Storage           (BlockStore (..), RepoStore (..))
import ATProto.PDS.Repo              (exportRepoCar, getRecord)
import ATProto.Syntax.DID            (DID, unDID, parseDID)
import ATProto.XRPC.Server.Types     (XrpcServerRequest (..), XrpcHandlerResult (..))
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- com.atproto.sync.getBlob
-- ---------------------------------------------------------------------------

handleGetBlob
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetBlob req = do
  let params = xsrParams req
      mCid   = Map.lookup "cid" params
  case mCid of
    Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "cid parameter required")
    Just cidText -> do
      case textToCidBytes cidText of
        Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid CID")
        Right cid -> do
          env <- asks id
          blobs <- liftIO $ readIORef (envBlobs env)
          case Map.lookup cid blobs of
            Nothing -> return $ XrpcHandlerError "BlobNotFound" (Just "Blob not found")
            Just blobBytes -> return $ XrpcSuccess (BL.fromStrict blobBytes)

-- ---------------------------------------------------------------------------
-- com.atproto.sync.getRecord
-- ---------------------------------------------------------------------------

handleSyncGetRecord
  :: BlockStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleSyncGetRecord req = do
  let params = xsrParams req
      mDid        = Map.lookup "did" params
      mCollection = Map.lookup "collection" params
      mRkey       = Map.lookup "rkey" params
  case (mDid, mCollection, mRkey) of
    (Just didText, Just collection, Just rkey) -> do
      env <- asks id
      case parseDID didText of
        Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid DID")
        Right did -> do
          mHead <- liftIO $ getRepoHead (envStore env) did
          case mHead of
            Nothing -> return $ XrpcHandlerError "RepoNotFound" (Just "Repository not found")
            Just headCid -> do
              res <- liftIO $ getRecord (envStore env) headCid collection rkey
              case res of
                Left _err -> return $ XrpcHandlerError "InternalError" (Just "Failed to read record")
                Right Nothing -> return $ XrpcHandlerError "RecordNotFound" (Just "Record not found")
                Right (Just recordBytes) ->
                  return $ XrpcSuccess (BL.fromStrict recordBytes)
    _ -> return $ XrpcHandlerError "InvalidRequest"
           (Just "did, collection, and rkey are required")

-- ---------------------------------------------------------------------------
-- com.atproto.sync.getRepo
-- ---------------------------------------------------------------------------

handleGetRepo
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetRepo req = do
  let mDid = Map.lookup "did" (xsrParams req)
  case mDid of
    Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "did parameter required")
    Just didText -> do
      env <- asks id
      case parseDID didText of
        Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid DID")
        Right did -> do
          res <- liftIO $ exportRepoCar (envStore env) did
          case res of
            Left _err -> return $ XrpcHandlerError "RepoNotFound" (Just "Repository not found")
            Right carBytes -> return $ XrpcSuccess (BL.fromStrict carBytes)

-- ---------------------------------------------------------------------------
-- com.atproto.sync.getRepoStatus
-- ---------------------------------------------------------------------------

handleGetRepoStatus
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetRepoStatus req = do
  let mDid = Map.lookup "did" (xsrParams req)
  case mDid of
    Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "did parameter required")
    Just didText -> do
      env <- asks id
      case parseDID didText of
        Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid DID")
        Right did -> do
          mHead <- liftIO $ getRepoHead (envStore env) did
          accts <- liftIO $ readIORef (envAccounts env)
          let active = case Map.lookup didText accts of
                Just ai -> aiActive ai
                Nothing -> True
              status = if active then ("active" :: T.Text) else "deactivated"
          case mHead of
            Nothing -> return $ XrpcHandlerError "RepoNotFound" (Just "Repository not found")
            Just headCid ->
              return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
                "{\"did\":\"" <> didText <> "\""
                <> ",\"active\":" <> if active then "true" else "false"
                <> ",\"status\":\"" <> status <> "\""
                <> ",\"rev\":\"" <> cidToText headCid <> "\"}"

-- ---------------------------------------------------------------------------
-- com.atproto.sync.listBlobs
-- ---------------------------------------------------------------------------

handleListBlobs
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleListBlobs _req = do
  env <- asks id
  blobs <- liftIO $ readIORef (envBlobs env)
  let cids = map cidToText (Map.keys blobs)
      arr  = "[" <> T.intercalate "," (map (\c -> "\"" <> c <> "\"") cids) <> "]"
  return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
    "{\"cids\":" <> arr <> "}"

-- ---------------------------------------------------------------------------
-- com.atproto.sync.listRepos
-- ---------------------------------------------------------------------------

handleListRepos
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleListRepos _req = do
  env <- asks id
  accts <- liftIO $ readIORef (envAccounts env)
  let repos = map (\(_didText, ai) ->
        let d = unDID (aiDid ai)
            active = aiActive ai
            status = if active then ("active" :: T.Text) else "deactivated"
        in "{\"did\":\"" <> d <> "\""
           <> ",\"active\":" <> (if active then "true" else "false")
           <> ",\"status\":\"" <> status <> "\"}"
        ) (Map.toList accts)
      arr = "[" <> T.intercalate "," repos <> "]"
  return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
    "{\"repos\":" <> arr <> "}"

-- ---------------------------------------------------------------------------
-- com.atproto.sync.subscribeRepos  (stub)
-- ---------------------------------------------------------------------------

handleSubscribeRepos
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleSubscribeRepos _req =
  return $ XrpcHandlerError "MethodNotImplemented"
    (Just "subscribeRepos requires WebSocket transport (not yet supported)")
