{-# LANGUAGE ScopedTypeVariables #-}
-- | @com.atproto.repo.*@ endpoint handlers.
module ATProto.PDS.Server.Endpoints.Repo
  ( handleCreateRecord
  , handleDeleteRecord
  , handleGetRecord
  , handleListRecords
  , handlePutRecord
  , handleApplyWrites
  , handleDescribeRepo
  , handleUploadBlob
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.IORef                 (readIORef, modifyIORef')
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import ATProto.Car.Cid               (CidBytes, cidForDagCbor, cidToText)
import ATProto.PDS.Storage           (BlockStore (..), RepoStore (..))
import ATProto.PDS.Repo              (createRecord, deleteRecord, getRecord,
                                      listRecords, applyWrites,
                                      WriteOp (..), PdsError (..))
import ATProto.Syntax.DID            (DID, unDID, parseDID)
import ATProto.Syntax.TID            (tidNow, unTID)
import ATProto.XRPC.Server.Types     (XrpcServerRequest (..), XrpcHandlerResult (..))
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- com.atproto.repo.createRecord
-- ---------------------------------------------------------------------------

handleCreateRecord
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleCreateRecord req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just callerDid ->
      case xsrBody req of
        Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "Request body required")
        Just body -> do
          let mRepo       = jsonTextField "repo" body
              mCollection = jsonTextField "collection" body
              mRkey       = jsonTextField "rkey" body
              mRecord     = jsonObjField "record" body
          case (mRepo, mCollection) of
            (Just _repo, Just collection) -> do
              env <- asks id
              case parseDID callerDid of
                Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid caller DID")
                Right did -> do
                  rkey <- case mRkey of
                    Just k  -> return k
                    Nothing -> liftIO $ unTID <$> tidNow
                  let recordBytes = maybe "{}" (BL.toStrict . BL.fromStrict . TE.encodeUtf8) mRecord
                  res <- liftIO $ createRecord (envStore env) did (envSigningKey env)
                           collection rkey recordBytes
                  case res of
                    Left err -> return $ pdsErrorResult err
                    Right _commitCid -> do
                      let uri = "at://" <> callerDid <> "/" <> collection <> "/" <> rkey
                          cid = cidToText (cidForDagCbor recordBytes)
                      return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
                        "{\"uri\":\"" <> uri <> "\",\"cid\":\"" <> cid <> "\"}"
            _ -> return $ XrpcHandlerError "InvalidRequest"
                   (Just "repo and collection are required")

-- ---------------------------------------------------------------------------
-- com.atproto.repo.deleteRecord
-- ---------------------------------------------------------------------------

handleDeleteRecord
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleDeleteRecord req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just callerDid ->
      case xsrBody req of
        Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "Request body required")
        Just body -> do
          let mCollection = jsonTextField "collection" body
              mRkey       = jsonTextField "rkey" body
          case (mCollection, mRkey) of
            (Just collection, Just rkey) -> do
              env <- asks id
              case parseDID callerDid of
                Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid caller DID")
                Right did -> do
                  res <- liftIO $ deleteRecord (envStore env) did (envSigningKey env)
                           collection rkey
                  case res of
                    Left err -> return $ pdsErrorResult err
                    Right _  -> return XrpcAccepted
            _ -> return $ XrpcHandlerError "InvalidRequest"
                   (Just "collection and rkey are required")

-- ---------------------------------------------------------------------------
-- com.atproto.repo.getRecord
-- ---------------------------------------------------------------------------

handleGetRecord
  :: BlockStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetRecord req = do
  let params = xsrParams req
      mRepo       = Map.lookup "repo" params
      mCollection = Map.lookup "collection" params
      mRkey       = Map.lookup "rkey" params
  case (mRepo, mCollection, mRkey) of
    (Just repo, Just collection, Just rkey) -> do
      env <- asks id
      case parseDID repo of
        Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid repo DID")
        Right did -> do
          mHead <- liftIO $ getRepoHead (envStore env) did
          case mHead of
            Nothing -> return $ XrpcHandlerError "RepoNotFound" (Just "Repository not found")
            Just headCid -> do
              res <- liftIO $ getRecord (envStore env) headCid collection rkey
              case res of
                Left err -> return $ pdsErrorResult err
                Right Nothing -> return $ XrpcHandlerError "RecordNotFound"
                  (Just "Record not found")
                Right (Just recordBytes) -> do
                  let uri = "at://" <> repo <> "/" <> collection <> "/" <> rkey
                      cid = cidToText (cidForDagCbor recordBytes)
                  return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
                    "{\"uri\":\"" <> uri <> "\""
                    <> ",\"cid\":\"" <> cid <> "\""
                    <> ",\"value\":" <> TE.decodeUtf8Lenient recordBytes <> "}"
    _ -> return $ XrpcHandlerError "InvalidRequest"
           (Just "repo, collection, and rkey are required")

-- ---------------------------------------------------------------------------
-- com.atproto.repo.listRecords
-- ---------------------------------------------------------------------------

handleListRecords
  :: BlockStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleListRecords req = do
  let params = xsrParams req
      mRepo       = Map.lookup "repo" params
      mCollection = Map.lookup "collection" params
  case (mRepo, mCollection) of
    (Just repo, Just collection) -> do
      env <- asks id
      case parseDID repo of
        Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid repo DID")
        Right did -> do
          mHead <- liftIO $ getRepoHead (envStore env) did
          case mHead of
            Nothing -> return $ XrpcHandlerError "RepoNotFound" (Just "Repository not found")
            Just headCid -> do
              res <- liftIO $ listRecords (envStore env) headCid collection
              case res of
                Left err -> return $ pdsErrorResult err
                Right records -> do
                  let entries = map (\(rk, bs) ->
                        let uri = "at://" <> repo <> "/" <> collection <> "/" <> rk
                            cid = cidToText (cidForDagCbor bs)
                        in "{\"uri\":\"" <> uri <> "\""
                           <> ",\"cid\":\"" <> cid <> "\""
                           <> ",\"value\":" <> TE.decodeUtf8Lenient bs <> "}"
                        ) records
                      arr = "[" <> T.intercalate "," entries <> "]"
                  return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
                    "{\"records\":" <> arr <> "}"
    _ -> return $ XrpcHandlerError "InvalidRequest"
           (Just "repo and collection are required")

-- ---------------------------------------------------------------------------
-- com.atproto.repo.putRecord
-- ---------------------------------------------------------------------------

handlePutRecord
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handlePutRecord req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just callerDid ->
      case xsrBody req of
        Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "Request body required")
        Just body -> do
          let mCollection = jsonTextField "collection" body
              mRkey       = jsonTextField "rkey" body
              mRecord     = jsonObjField "record" body
          case (mCollection, mRkey) of
            (Just collection, Just rkey) -> do
              env <- asks id
              case parseDID callerDid of
                Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid caller DID")
                Right did -> do
                  let recordBytes = maybe "{}" (BL.toStrict . BL.fromStrict . TE.encodeUtf8) mRecord
                  res <- liftIO $ applyWrites (envStore env) did (envSigningKey env)
                           [Update collection rkey recordBytes]
                  case res of
                    Left err -> return $ pdsErrorResult err
                    Right _commitCid -> do
                      let uri = "at://" <> callerDid <> "/" <> collection <> "/" <> rkey
                          cid = cidToText (cidForDagCbor recordBytes)
                      return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
                        "{\"uri\":\"" <> uri <> "\",\"cid\":\"" <> cid <> "\"}"
            _ -> return $ XrpcHandlerError "InvalidRequest"
                   (Just "collection and rkey are required")

-- ---------------------------------------------------------------------------
-- com.atproto.repo.applyWrites
-- ---------------------------------------------------------------------------

handleApplyWrites
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleApplyWrites req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just callerDid ->
      case xsrBody req of
        Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "Request body required")
        Just body -> do
          let mWrites = jsonArrayField "writes" body
          case mWrites of
            Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "writes array required")
            Just writes -> do
              env <- asks id
              case parseDID callerDid of
                Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid caller DID")
                Right did -> do
                  let ops = concatMap parseWriteOp writes
                  res <- liftIO $ applyWrites (envStore env) did (envSigningKey env) ops
                  case res of
                    Left err -> return $ pdsErrorResult err
                    Right _  -> return $ XrpcSuccess "{}"
  where
    parseWriteOp :: T.Text -> [WriteOp]
    parseWriteOp opText =
      let mType       = jsonTextField "$type" (BL.fromStrict (TE.encodeUtf8 opText))
          mCollection = jsonTextField "collection" (BL.fromStrict (TE.encodeUtf8 opText))
          mRkey       = jsonTextField "rkey" (BL.fromStrict (TE.encodeUtf8 opText))
          mRecord     = jsonObjField "value" (BL.fromStrict (TE.encodeUtf8 opText))
      in case (mType, mCollection, mRkey) of
        (Just "com.atproto.repo.applyWrites#create", Just col, Just rk) ->
          let recordBytes = maybe "{}" (BL.toStrict . BL.fromStrict . TE.encodeUtf8) mRecord
          in [Create col rk recordBytes]
        (Just "com.atproto.repo.applyWrites#update", Just col, Just rk) ->
          let recordBytes = maybe "{}" (BL.toStrict . BL.fromStrict . TE.encodeUtf8) mRecord
          in [Update col rk recordBytes]
        (Just "com.atproto.repo.applyWrites#delete", Just col, Just rk) ->
          [Delete col rk]
        _ -> []

-- ---------------------------------------------------------------------------
-- com.atproto.repo.describeRepo
-- ---------------------------------------------------------------------------

handleDescribeRepo
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleDescribeRepo req = do
  let params = xsrParams req
      mRepo  = Map.lookup "repo" params
  case mRepo of
    Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "repo parameter required")
    Just repo -> do
      env <- asks id
      case parseDID repo of
        Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid repo DID")
        Right did -> do
          mHead <- liftIO $ getRepoHead (envStore env) did
          case mHead of
            Nothing -> return $ XrpcHandlerError "RepoNotFound" (Just "Repository not found")
            Just _ -> do
              mHandle <- liftIO $ lookupHandle env did
              let handle = maybe repo id mHandle
              return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
                "{\"handle\":\"" <> handle <> "\""
                <> ",\"did\":\"" <> repo <> "\""
                <> ",\"didDoc\":{}"
                <> ",\"collections\":[]"
                <> ",\"handleIsCorrect\":true}"

-- ---------------------------------------------------------------------------
-- com.atproto.repo.uploadBlob
-- ---------------------------------------------------------------------------

handleUploadBlob
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleUploadBlob req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just _callerDid ->
      case xsrBody req of
        Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "Request body required")
        Just body -> do
          env <- asks id
          let blobBytes = BL.toStrict body
              blobCid   = cidForDagCbor blobBytes
          liftIO $ modifyIORef' (envBlobs env) (Map.insert blobCid blobBytes)
          let cid = cidToText blobCid
          return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
            "{\"blob\":{\"$type\":\"blob\""
            <> ",\"ref\":{\"$link\":\"" <> cid <> "\"}"
            <> ",\"mimeType\":\"application/octet-stream\""
            <> ",\"size\":" <> T.pack (show (BS.length blobBytes)) <> "}}"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

pdsErrorResult :: PdsError -> XrpcHandlerResult
pdsErrorResult (PdsRepoNotFound t)      = XrpcHandlerError "RepoNotFound" (Just t)
pdsErrorResult (PdsRepoAlreadyExists t) = XrpcHandlerError "RepoAlreadyExists" (Just t)
pdsErrorResult (PdsBlockNotFound t)     = XrpcHandlerError "BlockNotFound" (Just t)
pdsErrorResult (PdsMstError t)          = XrpcHandlerError "InternalError" (Just t)
pdsErrorResult (PdsCommitDecodeError t) = XrpcHandlerError "InternalError" (Just t)
pdsErrorResult (PdsRecordExists t)      = XrpcHandlerError "RecordExists" (Just t)
pdsErrorResult (PdsRecordNotFound t)    = XrpcHandlerError "RecordNotFound" (Just t)

-- | Extract a text field from a JSON object (lazy ByteString).
jsonTextField :: T.Text -> BL.ByteString -> Maybe T.Text
jsonTextField field bs =
  let txt = TE.decodeUtf8Lenient (BL.toStrict bs)
      needle = "\"" <> field <> "\""
  in case T.breakOn needle txt of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise ->
          let afterKey = T.drop (T.length needle) rest
              afterColon = T.dropWhile (\c -> c == ':' || c == ' ') afterKey
          in case T.uncons afterColon of
            Just ('"', remainder) ->
              Just (T.takeWhile (/= '"') remainder)
            _ -> Nothing

-- | Extract a JSON object field value as text (including braces).
jsonObjField :: T.Text -> BL.ByteString -> Maybe T.Text
jsonObjField field bs =
  let txt = TE.decodeUtf8Lenient (BL.toStrict bs)
      needle = "\"" <> field <> "\""
  in case T.breakOn needle txt of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise ->
          let afterKey = T.drop (T.length needle) rest
              afterColon = T.dropWhile (\c -> c == ':' || c == ' ') afterKey
          in case T.uncons afterColon of
            Just ('{', _) -> Just (extractBraced afterColon)
            _             -> Nothing

-- | Extract a JSON array field as list of text items (each an object).
jsonArrayField :: T.Text -> BL.ByteString -> Maybe [T.Text]
jsonArrayField field bs =
  let txt = TE.decodeUtf8Lenient (BL.toStrict bs)
      needle = "\"" <> field <> "\""
  in case T.breakOn needle txt of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise ->
          let afterKey = T.drop (T.length needle) rest
              afterColon = T.dropWhile (\c -> c == ':' || c == ' ') afterKey
          in case T.uncons afterColon of
            Just ('[', inner) -> Just (splitArrayObjects (T.dropEnd 1 (T.takeWhile (/= ']') inner <> "]")))
            _ -> Just []

-- | Split a JSON array of objects into individual object texts.
splitArrayObjects :: T.Text -> [T.Text]
splitArrayObjects t
  | T.null stripped = []
  | otherwise = go stripped
  where
    stripped = T.strip t
    go s = case T.uncons s of
      Just ('{', _) ->
        let obj = extractBraced s
            remaining = T.drop (T.length obj) s
            rest = T.dropWhile (\c -> c == ',' || c == ' ') remaining
        in obj : go rest
      _ -> []

-- | Extract a brace-delimited JSON value.
extractBraced :: T.Text -> T.Text
extractBraced = go 0 T.empty
  where
    go :: Int -> T.Text -> T.Text -> T.Text
    go depth acc s = case T.uncons s of
      Nothing -> acc
      Just (c, rest)
        | c == '{' -> go (depth + 1) (T.snoc acc c) rest
        | c == '}' ->
            if depth <= 1
              then T.snoc acc c
              else go (depth - 1) (T.snoc acc c) rest
        | otherwise -> go depth (T.snoc acc c) rest
