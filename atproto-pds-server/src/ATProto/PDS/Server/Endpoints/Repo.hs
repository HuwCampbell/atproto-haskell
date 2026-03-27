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
import           Data.IORef                 (modifyIORef')
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.Car.Cid               (cidForDagCbor, cidToText)
import ATProto.Ipld.Value            (LexValue, BlobRef (..),  Cid (..))
import ATProto.Lex.Codec             (Codec)
import qualified ATProto.Lex.Codec    as Codec
import qualified ATProto.Lex.Json     as LexJson
import qualified ATProto.Lex.Cbor     as LexCbor
import ATProto.PDS.Storage           (BlockStore (..), RepoStore (..))
import ATProto.PDS.Repo              (PdsError (..))
import qualified ATProto.PDS.Repo     as PDS
import ATProto.Repo.CreateRecord     (CreateRecordRequest (..), CreateRecordResponse (..),
                                      createRecordRequestCodec, createRecordResponseCodec)
import ATProto.Repo.DeleteRecord     (DeleteRecordRequest (..), deleteRecordRequestCodec)
import ATProto.Repo.GetRecord        (GetRecordResponse (..), getRecordResponseCodec)
import ATProto.Repo.ListRecords      (ListRecordsResponse (..), RepoRecord (..),
                                      listRecordsResponseCodec)
import ATProto.Repo.PutRecord        (PutRecordRequest (..), PutRecordResponse (..),
                                      putRecordRequestCodec, putRecordResponseCodec)
import ATProto.Repo.UploadBlob       (UploadBlobResponse (..), uploadBlobResponseCodec)
import ATProto.Repo.CommitMeta       (CommitMeta (..), commitMetaCodec)
import ATProto.Syntax.DID            (unDID, parseDID)
import ATProto.Syntax.TID            (tidNow, unTID)
import ATProto.XRPC.Server.Types     (XrpcServerRequest (..), XrpcHandlerResult (..))
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- Types and codecs for endpoints without repo-package types
-- ---------------------------------------------------------------------------

data DescribeRepoResponse = DescribeRepoResponse
  { drrHandle          :: T.Text
  , drrDid             :: T.Text
  , drrHandleIsCorrect :: Bool
  , drrCollections     :: [T.Text]
  }

describeRepoResponseCodec :: Codec DescribeRepoResponse
describeRepoResponseCodec =
    Codec.record "com.atproto.repo.describeRepo#response" $
        DescribeRepoResponse
            <$> Codec.requiredField "handle"          Codec.handle        drrHandle
            <*> Codec.requiredField "did"             Codec.did           drrDid
            <*> Codec.requiredField "handleIsCorrect" Codec.bool          drrHandleIsCorrect
            <*> Codec.requiredField "collections"     (Codec.array Codec.text) drrCollections

data ApplyWritesRequest = ApplyWritesRequest
  { awrRepo   :: T.Text
  , awrWrites :: [LexValue]
  }

applyWritesRequestCodec :: Codec ApplyWritesRequest
applyWritesRequestCodec =
    Codec.record "com.atproto.repo.applyWrites" $
        ApplyWritesRequest
            <$> Codec.requiredField "repo"   Codec.text                       awrRepo
            <*> Codec.requiredField "writes" (Codec.array Codec.lexValue)     awrWrites

-- | Serialise a LexValue to DAG-CBOR bytes for storage.
lexToCbor :: LexValue -> BS.ByteString
lexToCbor = LexCbor.serialise Codec.lexValue

-- | Deserialise DAG-CBOR bytes back to a LexValue for JSON responses.
cborToLex :: BS.ByteString -> Maybe LexValue
cborToLex bs = case LexCbor.deserialise Codec.lexValue bs of
  Right v -> Just v
  Left  _ -> Nothing

-- | Build a dummy commit meta from a CID.
dummyCommitMeta :: T.Text -> CommitMeta
dummyCommitMeta cid = CommitMeta cid cid

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
        Just body ->
          case LexJson.decode createRecordRequestCodec body of
            Left err -> return $ XrpcHandlerError "InvalidRequest" (Just (T.pack err))
            Right parsed -> do
              env <- asks id
              case parseDID callerDid of
                Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid caller DID")
                Right did -> do
                  let recordBytes = lexToCbor (crrRecord parsed)
                      collection  = crrCollection parsed
                      rkey        = crrRkey parsed
                  res <- liftIO $ PDS.createRecord (envStore env) did (envSigningKey env)
                           collection rkey recordBytes
                  case res of
                    Left err -> return $ pdsErrorResult err
                    Right commitCid -> do
                      let uri  = "at://" <> callerDid <> "/" <> collection <> "/" <> rkey
                          cid  = cidToText (cidForDagCbor recordBytes)
                          resp = CreateRecordResponse uri cid (dummyCommitMeta (cidToText commitCid))
                      return $ XrpcSuccess (LexJson.encode createRecordResponseCodec resp)

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
        Just body ->
          case LexJson.decode deleteRecordRequestCodec body of
            Left err -> return $ XrpcHandlerError "InvalidRequest" (Just (T.pack err))
            Right parsed -> do
              env <- asks id
              case parseDID callerDid of
                Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid caller DID")
                Right did -> do
                  res <- liftIO $ PDS.deleteRecord (envStore env) did (envSigningKey env)
                           (drrCollection parsed) (drrRkey parsed)
                  case res of
                    Left err -> return $ pdsErrorResult err
                    Right _  -> return XrpcAccepted
  where
    drrCollection = ATProto.Repo.DeleteRecord.drrCollection
    drrRkey       = ATProto.Repo.DeleteRecord.drrRkey

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
              res <- liftIO $ PDS.getRecord (envStore env) headCid collection rkey
              case res of
                Left err -> return $ pdsErrorResult err
                Right Nothing -> return $ XrpcHandlerError "RecordNotFound" (Just "Record not found")
                Right (Just recordBytes) -> do
                  let uri = "at://" <> repo <> "/" <> collection <> "/" <> rkey
                      cid = cidToText (cidForDagCbor recordBytes)
                      val = maybe (Codec.writer Codec.text "{}") id (cborToLex recordBytes)
                      resp = GetRecordResponse uri cid val
                  return $ XrpcSuccess (LexJson.encode getRecordResponseCodec resp)
    _ -> return $ XrpcHandlerError "InvalidRequest"
           (Just "repo, collection, and rkey are required")

-- ---------------------------------------------------------------------------
-- com.atproto.repo.listRecords
-- ---------------------------------------------------------------------------

handleListRecords
  :: (BlockStore s, RepoStore s)
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
              res <- liftIO $ PDS.listRecords (envStore env) headCid collection
              case res of
                Left err -> return $ pdsErrorResult err
                Right records -> do
                  let entries = map (\(rk, bs) ->
                        let uri = "at://" <> repo <> "/" <> collection <> "/" <> rk
                            cid = cidToText (cidForDagCbor bs)
                            val = maybe (Codec.writer Codec.text "{}") id (cborToLex bs)
                        in RepoRecord uri cid val
                        ) records
                      resp = ListRecordsResponse Nothing entries
                  return $ XrpcSuccess (LexJson.encode listRecordsResponseCodec resp)
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
        Just body ->
          case LexJson.decode putRecordRequestCodec body of
            Left err -> return $ XrpcHandlerError "InvalidRequest" (Just (T.pack err))
            Right parsed -> do
              env <- asks id
              case parseDID callerDid of
                Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid caller DID")
                Right did -> do
                  let recordBytes = lexToCbor (prrRecord parsed)
                      collection  = prrCollection parsed
                      rkey        = prrRkey parsed
                  res <- liftIO $ PDS.applyWrites (envStore env) did (envSigningKey env)
                           [PDS.Update collection rkey recordBytes]
                  case res of
                    Left err -> return $ pdsErrorResult err
                    Right commitCid -> do
                      let uri  = "at://" <> callerDid <> "/" <> collection <> "/" <> rkey
                          cid  = cidToText (cidForDagCbor recordBytes)
                          resp = PutRecordResponse uri cid (dummyCommitMeta (cidToText commitCid))
                      return $ XrpcSuccess (LexJson.encode putRecordResponseCodec resp)

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
        Just body ->
          case LexJson.decode applyWritesRequestCodec body of
            Left err -> return $ XrpcHandlerError "InvalidRequest" (Just (T.pack err))
            Right parsed -> do
              env <- asks id
              case parseDID callerDid of
                Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid caller DID")
                Right did -> do
                  let ops = concatMap parseWriteOp (awrWrites parsed)
                  res <- liftIO $ PDS.applyWrites (envStore env) did (envSigningKey env) ops
                  case res of
                    Left err -> return $ pdsErrorResult err
                    Right _  -> return $ XrpcSuccess "{}"
  where
    parseWriteOp :: LexValue -> [PDS.WriteOp]
    parseWriteOp val = case val of
      Codec.writer -> []  -- shouldn't happen
      _ -> []  -- Simplified: real parsing would extract $type/collection/rkey/value

-- ---------------------------------------------------------------------------
-- com.atproto.repo.describeRepo
-- ---------------------------------------------------------------------------

handleDescribeRepo
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleDescribeRepo req = do
  let mRepo = Map.lookup "repo" (xsrParams req)
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
                  resp = DescribeRepoResponse handle repo True []
              return $ XrpcSuccess (LexJson.encode describeRepoResponseCodec resp)

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
          let blobRef = BlobRef (Cid (cidToText blobCid)) "application/octet-stream"
                          (fromIntegral (BS.length blobBytes))
              resp = UploadBlobResponse blobRef
          return $ XrpcSuccess (LexJson.encode uploadBlobResponseCodec resp)

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
