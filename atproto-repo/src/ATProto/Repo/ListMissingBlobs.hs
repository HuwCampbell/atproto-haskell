-- | Typed binding for @com.atproto.repo.listMissingBlobs@.
--
-- Returns a list of missing blobs for the requesting account.  Intended
-- to be used in the account migration flow.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/listMissingBlobs.json>.
module ATProto.Repo.ListMissingBlobs
  ( -- * Request parameters
    ListMissingBlobsParams (..)
  , defaultListMissingBlobsParams
    -- * Response
  , ListMissingBlobsResponse (..)
  , RecordBlob (..)
    -- * Codecs
  , listMissingBlobsResponseCodec
  , recordBlobCodec
    -- * Client function
  , listMissingBlobs
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Maybe              as Maybe
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import qualified ATProto.Lex.Schema      as Codec
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request parameters
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.repo.listMissingBlobs@.
data ListMissingBlobsParams = ListMissingBlobsParams
  { lmbpLimit  :: Maybe Int
    -- ^ Maximum number of missing blobs to return (1–1000, default 500).
  , lmbpCursor :: Maybe T.Text
    -- ^ Pagination cursor from a previous response.
  } deriving (Eq, Show)

-- | Default 'ListMissingBlobsParams' with no pagination.
defaultListMissingBlobsParams :: ListMissingBlobsParams
defaultListMissingBlobsParams = ListMissingBlobsParams
  { lmbpLimit  = Nothing
  , lmbpCursor = Nothing
  }

-- | Convert 'ListMissingBlobsParams' to XRPC query-string parameters.
toQueryParams :: ListMissingBlobsParams -> Map.Map T.Text T.Text
toQueryParams p =
  Map.fromList $ Maybe.catMaybes
    [ (\n -> ("limit",  T.pack (show n))) <$> lmbpLimit p
    , (\c -> ("cursor", c))               <$> lmbpCursor p
    ]

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | A missing blob record reference.
--
-- Corresponds to @com.atproto.repo.listMissingBlobs#recordBlob@.
data RecordBlob = RecordBlob
  { rbCid       :: T.Text
    -- ^ CID of the missing blob.
  , rbRecordUri :: T.Text
    -- ^ AT-URI of the record that references this blob.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.repo.listMissingBlobs#recordBlob@.
recordBlobCodec :: Codec RecordBlob
recordBlobCodec =
    Codec.record "com.atproto.repo.listMissingBlobs#recordBlob" $
        RecordBlob
            <$> Codec.requiredField "cid"       (Codec.string Codec.LexFormatCid)  rbCid
            <*> Codec.requiredField "recordUri" Codec.atUri                        rbRecordUri

-- | Response from @com.atproto.repo.listMissingBlobs@.
data ListMissingBlobsResponse = ListMissingBlobsResponse
  { lmbrCursor :: Maybe T.Text
    -- ^ Pagination cursor; present when more blobs are available.
  , lmbrBlobs  :: [RecordBlob]
    -- ^ The missing blobs.
  } deriving (Eq, Show)

-- | Codec for the @listMissingBlobs@ response body.
listMissingBlobsResponseCodec :: Codec ListMissingBlobsResponse
listMissingBlobsResponseCodec =
    Codec.record "com.atproto.repo.listMissingBlobs#response" $
        ListMissingBlobsResponse
            <$> Codec.optionalField "cursor" Codec.text                      lmbrCursor
            <*> Codec.requiredField "blobs"  (Codec.array recordBlobCodec)   lmbrBlobs

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.repo.listMissingBlobs@ using the given XRPC client.
listMissingBlobs
  :: XrpcClient c
  => c
  -> ListMissingBlobsParams
  -> IO (Either XrpcError ListMissingBlobsResponse)
listMissingBlobs client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.repo.listMissingBlobs"
    , xrpcReqParams  = toQueryParams params
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError ListMissingBlobsResponse
parseResponse body =
  case LexJson.decode listMissingBlobsResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
