{-# LANGUAGE LambdaCase #-}
-- | Typed binding for @com.atproto.repo.applyWrites@.
--
-- Apply a batch transaction of repository creates, updates, and deletes.
-- Requires auth, implemented by PDS.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/applyWrites.json>.
module ATProto.Repo.ApplyWrites
  ( -- * Request
    ApplyWritesRequest (..)
    -- * Write operations
  , ApplyWritesOp (..)
  , ApplyWritesCreate (..)
  , ApplyWritesUpdate (..)
  , ApplyWritesDelete (..)
    -- * Response
  , ApplyWritesResponse (..)
  , ApplyWritesResult (..)
  , ApplyWritesCreateResult (..)
  , ApplyWritesUpdateResult (..)
  , ApplyWritesDeleteResult (..)
    -- * Codecs
  , applyWritesRequestCodec
  , applyWritesResponseCodec
  , applyWritesOpCodec
  , applyWritesCreateCodec
  , applyWritesUpdateCodec
  , applyWritesDeleteCodec
  , applyWritesResultCodec
  , applyWritesCreateResultCodec
  , applyWritesUpdateResultCodec
  , applyWritesDeleteResultCodec
    -- * Client function
  , applyWrites
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import qualified ATProto.Lex.Schema      as Codec
import           ATProto.Repo.CommitMeta (CommitMeta, commitMetaCodec)
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Write operations
-- ---------------------------------------------------------------------------

-- | Operation which creates a new record.
--
-- Corresponds to @com.atproto.repo.applyWrites#create@.
data ApplyWritesCreate = ApplyWritesCreate
  { awcCollection :: T.Text
    -- ^ NSID of the collection.
  , awcRkey       :: Maybe T.Text
    -- ^ Record key.
  , awcValue      :: LexValue
    -- ^ The record value to write.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.repo.applyWrites#create@.
applyWritesCreateCodec :: Codec ApplyWritesCreate
applyWritesCreateCodec =
    Codec.record "com.atproto.repo.applyWrites#create" $
        ApplyWritesCreate
            <$> Codec.requiredField "collection" Codec.text      awcCollection
            <*> Codec.optionalField "rkey"       Codec.text      awcRkey
            <*> Codec.requiredField "value"      Codec.lexValue  awcValue

-- | Operation which updates an existing record.
--
-- Corresponds to @com.atproto.repo.applyWrites#update@.
data ApplyWritesUpdate = ApplyWritesUpdate
  { awuCollection :: T.Text
    -- ^ NSID of the collection.
  , awuRkey       :: T.Text
    -- ^ Record key.
  , awuValue      :: LexValue
    -- ^ The new record value.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.repo.applyWrites#update@.
applyWritesUpdateCodec :: Codec ApplyWritesUpdate
applyWritesUpdateCodec =
    Codec.record "com.atproto.repo.applyWrites#update" $
        ApplyWritesUpdate
            <$> Codec.requiredField "collection" Codec.text      awuCollection
            <*> Codec.requiredField "rkey"       Codec.text      awuRkey
            <*> Codec.requiredField "value"      Codec.lexValue  awuValue

-- | Operation which deletes an existing record.
--
-- Corresponds to @com.atproto.repo.applyWrites#delete@.
data ApplyWritesDelete = ApplyWritesDelete
  { awdCollection :: T.Text
    -- ^ NSID of the collection.
  , awdRkey       :: T.Text
    -- ^ Record key.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.repo.applyWrites#delete@.
applyWritesDeleteCodec :: Codec ApplyWritesDelete
applyWritesDeleteCodec =
    Codec.record "com.atproto.repo.applyWrites#delete" $
        ApplyWritesDelete
            <$> Codec.requiredField "collection" Codec.text  awdCollection
            <*> Codec.requiredField "rkey"       Codec.text  awdRkey

-- | A single write operation (create, update, or delete).
data ApplyWritesOp
  = ApplyWritesOpCreate ApplyWritesCreate
  | ApplyWritesOpUpdate ApplyWritesUpdate
  | ApplyWritesOpDelete ApplyWritesDelete
  deriving (Eq, Show)

-- | Codec for the write operation union.
applyWritesOpCodec :: Codec ApplyWritesOp
applyWritesOpCodec =
    let from = \case
          ApplyWritesOpCreate c -> Left c
          ApplyWritesOpUpdate u -> Right (Left u)
          ApplyWritesOpDelete d -> Right (Right d)
        to = \case
          Left c          -> ApplyWritesOpCreate c
          Right (Left u)  -> ApplyWritesOpUpdate u
          Right (Right d) -> ApplyWritesOpDelete d
    in Codec.invmap to from $
       Codec.union3
         applyWritesCreateCodec
         applyWritesUpdateCodec
         applyWritesDeleteCodec

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Input body for @com.atproto.repo.applyWrites@.
data ApplyWritesRequest = ApplyWritesRequest
  { awrRepo       :: T.Text
    -- ^ The handle or DID of the repo (aka, current account).
  , awrWrites     :: [ApplyWritesOp]
    -- ^ The batch of write operations.
  , awrValidate   :: Maybe Bool
    -- ^ Whether to validate records against Lexicon schemas.
  , awrSwapCommit :: Maybe T.Text
    -- ^ If provided, the operation will fail if the current repo commit CID
    -- does not match.
  } deriving (Eq, Show)

-- | Codec for the @applyWrites@ request body.
applyWritesRequestCodec :: Codec ApplyWritesRequest
applyWritesRequestCodec =
    Codec.record "com.atproto.repo.applyWrites#request" $
        ApplyWritesRequest
            <$> Codec.requiredField "repo"       Codec.text                          awrRepo
            <*> Codec.requiredField "writes"     (Codec.array applyWritesOpCodec)    awrWrites
            <*> Codec.optionalField "validate"   Codec.bool                          awrValidate
            <*> Codec.optionalField "swapCommit" (Codec.string Codec.LexFormatCid)   awrSwapCommit

-- ---------------------------------------------------------------------------
-- Result types
-- ---------------------------------------------------------------------------

-- | Result of a create write operation.
--
-- Corresponds to @com.atproto.repo.applyWrites#createResult@.
data ApplyWritesCreateResult = ApplyWritesCreateResult
  { awcrUri              :: T.Text
    -- ^ AT-URI of the created record.
  , awcrCid              :: T.Text
    -- ^ CID of the created record.
  , awcrValidationStatus :: Maybe T.Text
    -- ^ Validation status (valid, unknown).
  } deriving (Eq, Show)

-- | Codec for @com.atproto.repo.applyWrites#createResult@.
applyWritesCreateResultCodec :: Codec ApplyWritesCreateResult
applyWritesCreateResultCodec =
    Codec.record "com.atproto.repo.applyWrites#createResult" $
        ApplyWritesCreateResult
            <$> Codec.requiredField "uri"              Codec.atUri  awcrUri
            <*> Codec.requiredField "cid"              (Codec.string Codec.LexFormatCid) awcrCid
            <*> Codec.optionalField "validationStatus" Codec.text   awcrValidationStatus

-- | Result of an update write operation.
--
-- Corresponds to @com.atproto.repo.applyWrites#updateResult@.
data ApplyWritesUpdateResult = ApplyWritesUpdateResult
  { awurUri              :: T.Text
    -- ^ AT-URI of the updated record.
  , awurCid              :: T.Text
    -- ^ CID of the updated record.
  , awurValidationStatus :: Maybe T.Text
    -- ^ Validation status (valid, unknown).
  } deriving (Eq, Show)

-- | Codec for @com.atproto.repo.applyWrites#updateResult@.
applyWritesUpdateResultCodec :: Codec ApplyWritesUpdateResult
applyWritesUpdateResultCodec =
    Codec.record "com.atproto.repo.applyWrites#updateResult" $
        ApplyWritesUpdateResult
            <$> Codec.requiredField "uri"              Codec.atUri  awurUri
            <*> Codec.requiredField "cid"              (Codec.string Codec.LexFormatCid) awurCid
            <*> Codec.optionalField "validationStatus" Codec.text   awurValidationStatus

-- | Result of a delete write operation.
--
-- Corresponds to @com.atproto.repo.applyWrites#deleteResult@.
data ApplyWritesDeleteResult = ApplyWritesDeleteResult
  deriving (Eq, Show)

-- | Codec for @com.atproto.repo.applyWrites#deleteResult@.
applyWritesDeleteResultCodec :: Codec ApplyWritesDeleteResult
applyWritesDeleteResultCodec =
    Codec.record "com.atproto.repo.applyWrites#deleteResult" $
        pure ApplyWritesDeleteResult

-- | A single result from an applyWrites batch.
data ApplyWritesResult
  = ApplyWritesResultCreate ApplyWritesCreateResult
  | ApplyWritesResultUpdate ApplyWritesUpdateResult
  | ApplyWritesResultDelete ApplyWritesDeleteResult
  deriving (Eq, Show)

-- | Codec for the result union.
applyWritesResultCodec :: Codec ApplyWritesResult
applyWritesResultCodec =
    let from = \case
          ApplyWritesResultCreate c -> Left c
          ApplyWritesResultUpdate u -> Right (Left u)
          ApplyWritesResultDelete d -> Right (Right d)
        to = \case
          Left c          -> ApplyWritesResultCreate c
          Right (Left u)  -> ApplyWritesResultUpdate u
          Right (Right d) -> ApplyWritesResultDelete d
    in Codec.invmap to from $
       Codec.union3
         applyWritesCreateResultCodec
         applyWritesUpdateResultCodec
         applyWritesDeleteResultCodec

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.repo.applyWrites@.
data ApplyWritesResponse = ApplyWritesResponse
  { awresCommit  :: Maybe CommitMeta
    -- ^ Commit metadata.
  , awresResults :: Maybe [ApplyWritesResult]
    -- ^ Per-operation results.
  } deriving (Eq, Show)

-- | Codec for the @applyWrites@ response body.
applyWritesResponseCodec :: Codec ApplyWritesResponse
applyWritesResponseCodec =
    Codec.record "com.atproto.repo.applyWrites#response" $
        ApplyWritesResponse
            <$> Codec.optionalField "commit"  commitMetaCodec                       awresCommit
            <*> Codec.optionalField "results" (Codec.array applyWritesResultCodec)  awresResults

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.repo.applyWrites@ using the given XRPC client.
--
-- Applies a batch of creates, updates, and deletes to the user's repository.
applyWrites
  :: XrpcClient c
  => c
  -> ApplyWritesRequest
  -> IO (Either XrpcError ApplyWritesResponse)
applyWrites client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.repo.applyWrites"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (LexJson.encode applyWritesRequestCodec req)
    , xrpcReqHeaders = Map.singleton "Content-Type" "application/json"
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError ApplyWritesResponse
parseResponse body =
  case LexJson.decode applyWritesResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
