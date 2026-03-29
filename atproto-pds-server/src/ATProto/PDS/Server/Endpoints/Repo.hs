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
import qualified Data.Text            as T

import ATProto.Car.Cid               (cidForDagCbor, cidToText)
import ATProto.Ipld.Value            (LexValue (..), BlobRef (..), Cid (..))
import qualified ATProto.Lex.Codec    as Codec
import qualified ATProto.Lex.Cbor     as LexCbor
import ATProto.PDS.Storage           (BlockStore (..), RepoStore (..),
                                      BlobStore (..), AccountStore (..),
                                      AccountInfo (..))
import ATProto.PDS.Repo              (PdsError (..))
import qualified ATProto.PDS.Repo     as PDS
import ATProto.Repo.ApplyWrites      (applyWritesRequestCodec)
import ATProto.Repo.CreateRecord     (CreateRecordRequest (..), CreateRecordResponse (..),
                                      createRecordRequestCodec, createRecordResponseCodec)
import ATProto.Repo.DeleteRecord     (DeleteRecordRequest (..),
                                      deleteRecordRequestCodec)
import ATProto.Repo.DescribeRepo     (DescribeRepoResponse (..),
                                      describeRepoResponseCodec)
import ATProto.Repo.GetRecord        (GetRecordResponse (..), getRecordResponseCodec)
import ATProto.Repo.ListRecords      (ListRecordsResponse (..), RepoRecord (..),
                                      listRecordsResponseCodec)
import ATProto.Repo.PutRecord        (PutRecordRequest (..), PutRecordResponse (..),
                                      putRecordRequestCodec, putRecordResponseCodec)
import ATProto.Repo.UploadBlob       (UploadBlobResponse (..), uploadBlobResponseCodec)
import ATProto.Repo.CommitMeta       (CommitMeta (..))
import ATProto.Syntax.DID            (unDID, parseDID)
import ATProto.XRPC.Server           (XrpcServerRequest (..), XrpcHandlerResult (..), lift,
                                      runHandler, requireAuth, requireParam,
                                      decodeBody, requireBody, respondCodec,
                                      respondRaw, throwXrpc, Handler)
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Serialise a LexValue to DAG-CBOR bytes for storage.
lexToCbor :: LexValue -> BS.ByteString
lexToCbor = BL.toStrict . LexCbor.serialise Codec.lexValue

-- | Deserialise DAG-CBOR bytes back to a LexValue for JSON responses.
cborToLex :: BS.ByteString -> LexValue
cborToLex bs = case LexCbor.deserialise Codec.lexValue (BL.fromStrict bs) of
  Right v -> v
  Left  _ -> LexNull

-- | Build a commit meta from a CID.
mkCommitMeta :: T.Text -> CommitMeta
mkCommitMeta cid = CommitMeta cid cid

pdsErrorToXrpc :: PdsError -> (T.Text, T.Text)
pdsErrorToXrpc (PdsRepoNotFound t)      = ("RepoNotFound", t)
pdsErrorToXrpc (PdsRepoAlreadyExists t) = ("RepoAlreadyExists", t)
pdsErrorToXrpc (PdsBlockNotFound t)     = ("BlockNotFound", t)
pdsErrorToXrpc (PdsMstError t)          = ("InternalError", t)
pdsErrorToXrpc (PdsCommitDecodeError t) = ("InternalError", t)
pdsErrorToXrpc (PdsRecordExists t)      = ("RecordExists", t)
pdsErrorToXrpc (PdsRecordNotFound t)    = ("RecordNotFound", t)

throwPds :: Monad m => PdsError -> Handler m a
throwPds err = let (c, m) = pdsErrorToXrpc err in throwXrpc c m

-- ---------------------------------------------------------------------------
-- com.atproto.repo.createRecord
-- ---------------------------------------------------------------------------

handleCreateRecord
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleCreateRecord req = runHandler $ do
  callerDid <- requireAuth req
  parsed <- decodeBody createRecordRequestCodec req
  did <- case parseDID callerDid of
    Left _  -> throwXrpc "InvalidRequest" "Invalid caller DID"
    Right d -> return d
  store <- lift $ asks envStore
  key <- lift $ asks envSigningKey
  let recordBytes = lexToCbor (crrRecord parsed)
      collection  = crrCollection parsed
      rkey        = crrRkey parsed
  res <- liftIO $ PDS.createRecord store did key collection rkey recordBytes
  case res of
    Left err -> throwPds err
    Right commitCid -> do
      let uri = "at://" <> callerDid <> "/" <> collection <> "/" <> rkey
          cid = cidToText (cidForDagCbor recordBytes)
      respondCodec createRecordResponseCodec $
        CreateRecordResponse uri cid (mkCommitMeta (cidToText commitCid))

-- ---------------------------------------------------------------------------
-- com.atproto.repo.deleteRecord
-- ---------------------------------------------------------------------------

handleDeleteRecord
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleDeleteRecord req = runHandler $ do
  callerDid <- requireAuth req
  parsed <- decodeBody deleteRecordRequestCodec req
  did <- case parseDID callerDid of
    Left _  -> throwXrpc "InvalidRequest" "Invalid caller DID"
    Right d -> return d
  store <- lift $ asks envStore
  key <- lift $ asks envSigningKey
  res <- liftIO $ PDS.deleteRecord store did key
           (ATProto.Repo.DeleteRecord.drrCollection parsed)
           (ATProto.Repo.DeleteRecord.drrRkey parsed)
  case res of
    Left err -> throwPds err
    Right _  -> return XrpcAccepted

-- ---------------------------------------------------------------------------
-- com.atproto.repo.getRecord
-- ---------------------------------------------------------------------------

handleGetRecord
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetRecord req = runHandler $ do
  repo       <- requireParam "repo" req
  collection <- requireParam "collection" req
  rkey       <- requireParam "rkey" req
  did <- case parseDID repo of
    Left _  -> throwXrpc "InvalidRequest" "Invalid repo DID"
    Right d -> return d
  store <- lift $ asks envStore
  mHead <- liftIO $ getRepoHead store did
  headCid <- case mHead of
    Nothing -> throwXrpc "RepoNotFound" "Repository not found"
    Just c  -> return c
  res <- liftIO $ PDS.getRecord store headCid collection rkey
  case res of
    Left err -> throwPds err
    Right Nothing -> throwXrpc "RecordNotFound" "Record not found"
    Right (Just recordBytes) -> do
      let uri = "at://" <> repo <> "/" <> collection <> "/" <> rkey
          cid = cidToText (cidForDagCbor recordBytes)
      respondCodec getRecordResponseCodec $
        GetRecordResponse uri cid (cborToLex recordBytes)

-- ---------------------------------------------------------------------------
-- com.atproto.repo.listRecords
-- ---------------------------------------------------------------------------

handleListRecords
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleListRecords req = runHandler $ do
  repo       <- requireParam "repo" req
  collection <- requireParam "collection" req
  did <- case parseDID repo of
    Left _  -> throwXrpc "InvalidRequest" "Invalid repo DID"
    Right d -> return d
  store <- lift $ asks envStore
  mHead <- liftIO $ getRepoHead store did
  headCid <- case mHead of
    Nothing -> throwXrpc "RepoNotFound" "Repository not found"
    Just c  -> return c
  res <- liftIO $ PDS.listRecords store headCid collection
  case res of
    Left err -> throwPds err
    Right records -> do
      let entries = map (\(rk, bs) ->
            let uri = "at://" <> repo <> "/" <> collection <> "/" <> rk
                cid = cidToText (cidForDagCbor bs)
            in RepoRecord uri cid (cborToLex bs)
            ) records
      respondCodec listRecordsResponseCodec $
        ListRecordsResponse Nothing entries

-- ---------------------------------------------------------------------------
-- com.atproto.repo.putRecord
-- ---------------------------------------------------------------------------

handlePutRecord
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handlePutRecord req = runHandler $ do
  callerDid <- requireAuth req
  parsed <- decodeBody putRecordRequestCodec req
  did <- case parseDID callerDid of
    Left _  -> throwXrpc "InvalidRequest" "Invalid caller DID"
    Right d -> return d
  store <- lift $ asks envStore
  key <- lift $ asks envSigningKey
  let recordBytes = lexToCbor (prrRecord parsed)
      collection  = prrCollection parsed
      rkey        = prrRkey parsed
  res <- liftIO $ PDS.applyWrites store did key
           [PDS.Update collection rkey recordBytes]
  case res of
    Left err -> throwPds err
    Right commitCid -> do
      let uri = "at://" <> callerDid <> "/" <> collection <> "/" <> rkey
          cid = cidToText (cidForDagCbor recordBytes)
      respondCodec putRecordResponseCodec $
        PutRecordResponse uri cid (mkCommitMeta (cidToText commitCid))

-- ---------------------------------------------------------------------------
-- com.atproto.repo.applyWrites
-- ---------------------------------------------------------------------------

handleApplyWrites
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleApplyWrites req = runHandler $ do
  callerDid <- requireAuth req
  _parsed <- decodeBody applyWritesRequestCodec req
  did <- case parseDID callerDid of
    Left _  -> throwXrpc "InvalidRequest" "Invalid caller DID"
    Right d -> return d
  store <- lift $ asks envStore
  key <- lift $ asks envSigningKey
  -- TODO: parse individual write ops from LexValue list
  res <- liftIO $ PDS.applyWrites store did key []
  case res of
    Left err -> throwPds err
    Right _  -> respondRaw "{}"

-- ---------------------------------------------------------------------------
-- com.atproto.repo.describeRepo
-- ---------------------------------------------------------------------------

handleDescribeRepo
  :: (BlockStore s, RepoStore s, AccountStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleDescribeRepo req = runHandler $ do
  repo <- requireParam "repo" req
  did <- case parseDID repo of
    Left _  -> throwXrpc "InvalidRequest" "Invalid repo DID"
    Right d -> return d
  store <- lift $ asks envStore
  mHead <- liftIO $ getRepoHead store did
  case mHead of
    Nothing -> throwXrpc "RepoNotFound" "Repository not found"
    Just _ -> do
      mAcct <- liftIO $ getAccount store (unDID did)
      let handle = maybe repo aiHandle mAcct
      respondCodec describeRepoResponseCodec $
        DescribeRepoResponse handle repo True []

-- ---------------------------------------------------------------------------
-- com.atproto.repo.uploadBlob
-- ---------------------------------------------------------------------------

handleUploadBlob
  :: BlobStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleUploadBlob req = runHandler $ do
  _callerDid <- requireAuth req
  body <- requireBody req
  store <- lift $ asks envStore
  let blobBytes = BL.toStrict body
      blobCid   = cidForDagCbor blobBytes
  liftIO $ putBlob store blobCid blobBytes
  let blobRef = BlobRef (Cid (cidToText blobCid)) "application/octet-stream"
                  (fromIntegral (BS.length blobBytes))
  respondCodec uploadBlobResponseCodec (UploadBlobResponse blobRef)
