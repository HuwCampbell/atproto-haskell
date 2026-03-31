-- | Typed binding for @com.atproto.sync.listBlobs@.
--
-- List blob CIDs for an account, since some repo revision.
-- Does not require auth; implemented by PDS.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/sync/listBlobs.json>.
module ATProto.Sync.ListBlobs
  ( -- * Request parameters
    ListBlobsParams (..)
  , defaultListBlobsParams
    -- * Response
  , ListBlobsResponse (..)
    -- * Codecs
  , listBlobsResponseCodec
    -- * Client function
  , listBlobs
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Maybe              as Maybe
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request parameters
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.sync.listBlobs@.
data ListBlobsParams = ListBlobsParams
  { lbpDid    :: T.Text
    -- ^ The DID of the repo.
  , lbpSince  :: Maybe T.Text
    -- ^ Optional revision of the repo to list blobs since.
  , lbpLimit  :: Maybe Int
    -- ^ Maximum number of blobs to return (1–1000, default 500).
  , lbpCursor :: Maybe T.Text
    -- ^ Pagination cursor from a previous response.
  } deriving (Eq, Show)

-- | A 'ListBlobsParams' with sensible defaults.
defaultListBlobsParams :: T.Text -> ListBlobsParams
defaultListBlobsParams did' = ListBlobsParams
  { lbpDid    = did'
  , lbpSince  = Nothing
  , lbpLimit  = Nothing
  , lbpCursor = Nothing
  }

-- | Convert 'ListBlobsParams' to XRPC query-string parameters.
toQueryParams :: ListBlobsParams -> Map.Map T.Text T.Text
toQueryParams p =
  Map.fromList $
    [ ("did", lbpDid p)
    ] ++ Maybe.catMaybes
    [ (\s -> ("since",  s))                <$> lbpSince p
    , (\n -> ("limit",  T.pack (show n)))  <$> lbpLimit p
    , (\c -> ("cursor", c))                <$> lbpCursor p
    ]

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.sync.listBlobs@.
data ListBlobsResponse = ListBlobsResponse
  { lbrCursor :: Maybe T.Text
    -- ^ Pagination cursor; present when more blobs are available.
  , lbrCids   :: [T.Text]
    -- ^ The blob CIDs.
  } deriving (Eq, Show)

-- | Codec for the @listBlobs@ response body.
listBlobsResponseCodec :: Codec ListBlobsResponse
listBlobsResponseCodec =
    Codec.record "com.atproto.sync.listBlobs#response" $
        ListBlobsResponse
            <$> Codec.optionalField "cursor" Codec.text              lbrCursor
            <*> Codec.requiredField "cids"   (Codec.array Codec.text) lbrCids

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.sync.listBlobs@ using the given XRPC client.
listBlobs
  :: XrpcClient c
  => c
  -> ListBlobsParams
  -> IO (Either XrpcError ListBlobsResponse)
listBlobs client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.sync.listBlobs"
    , xrpcReqParams  = toQueryParams params
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError ListBlobsResponse
parseResponse body =
  case LexJson.decode listBlobsResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
