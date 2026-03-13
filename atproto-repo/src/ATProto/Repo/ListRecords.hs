-- | Typed binding for @com.atproto.repo.listRecords@.
--
-- This module provides Haskell types for the query parameters and JSON
-- response of the @com.atproto.repo.listRecords@ XRPC method, plus a
-- convenience function 'listRecords' that works with any 'XrpcClient'.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/listRecords.json>.
module ATProto.Repo.ListRecords
  ( -- * Request parameters
    ListRecordsParams (..)
  , defaultListRecordsParams
  , toQueryParams
    -- * Response
  , ListRecordsResponse (..)
  , RepoRecord (..)
    -- * Codecs
  , repoRecordCodec
  , listRecordsResponseCodec
    -- * Client function
  , listRecords
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import ATProto.XRPC.Client (XrpcClient (..))
import ATProto.XRPC.Types  (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.repo.listRecords@.
--
-- 'repo' and 'collection' are required; all other fields are optional.
data ListRecordsParams = ListRecordsParams
  { lrpRepo       :: T.Text
    -- ^ The DID or handle of the repository, e.g.
    -- @\"did:plc:ewvi7nxzyoun6zhhandbv25b\"@ or @\"haileyok.com\"@.
  , lrpCollection :: T.Text
    -- ^ NSID of the record type, e.g. @\"app.bsky.feed.post\"@.
  , lrpLimit      :: Maybe Int
    -- ^ Maximum number of records to return (1–100, default 50).
  , lrpCursor     :: Maybe T.Text
    -- ^ Pagination cursor from a previous response.
  , lrpReverse    :: Maybe Bool
    -- ^ When 'True', return records in reverse chronological order.
  } deriving (Eq, Show)

-- | A 'ListRecordsParams' with sensible defaults (limit = 50, no pagination).
defaultListRecordsParams :: T.Text -> T.Text -> ListRecordsParams
defaultListRecordsParams repo collection = ListRecordsParams
  { lrpRepo       = repo
  , lrpCollection = collection
  , lrpLimit      = Nothing
  , lrpCursor     = Nothing
  , lrpReverse    = Nothing
  }

-- | Convert 'ListRecordsParams' to XRPC query-string parameters.
toQueryParams :: ListRecordsParams -> Map.Map T.Text T.Text
toQueryParams p =
  Map.fromList $
    [ ("repo",       lrpRepo p)
    , ("collection", lrpCollection p)
    ]
    ++ maybe [] (\n -> [("limit",  T.pack (show n))]) (lrpLimit p)
    ++ maybe [] (\c -> [("cursor", c)])                (lrpCursor p)
    ++ maybe [] (\r -> [("reverse", if r then "true" else "false")]) (lrpReverse p)

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | A single record returned by @listRecords@.
data RepoRecord = RepoRecord
  { rrUri   :: T.Text
    -- ^ AT-URI of the record, e.g.
    -- @\"at:\/\/did:plc:abc\/app.bsky.feed.post\/3k7bj\"@.
  , rrCid   :: T.Text
    -- ^ CID of the record's current version.
  , rrValue :: LexValue
    -- ^ The raw record value (type depends on the collection).
  } deriving (Eq, Show)

-- | The response body from @com.atproto.repo.listRecords@.
data ListRecordsResponse = ListRecordsResponse
  { lrrCursor  :: Maybe T.Text
    -- ^ Pagination cursor; present when more records are available.
  , lrrRecords :: [RepoRecord]
    -- ^ The returned records.
  } deriving (Eq, Show)

-- | Codec for a single @listRecords@ record entry.
repoRecordCodec :: Codec RepoRecord
repoRecordCodec =
    Codec.record "com.atproto.repo.listRecords#repoRecord" $
        RepoRecord
            <$> Codec.requiredField "uri"   Codec.atUri     rrUri
            <*> Codec.requiredField "cid"   Codec.text      rrCid
            <*> Codec.requiredField "value" Codec.lexValue  rrValue

-- | Codec for the @listRecords@ response body.
listRecordsResponseCodec :: Codec ListRecordsResponse
listRecordsResponseCodec =
    Codec.record "com.atproto.repo.listRecords#response" $
        ListRecordsResponse
            <$> Codec.optionalField "cursor"  Codec.text                          lrrCursor
            <*> Codec.requiredField "records" (Codec.array repoRecordCodec)       lrrRecords

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.repo.listRecords@ using the given XRPC client.
--
-- Returns 'Right' with the parsed response on success, or 'Left' with an
-- 'XrpcError' on failure.  The response body is decoded from JSON; a parse
-- failure is reported as an 'XrpcError' with the token @\"ParseError\"@.
--
-- = Example
--
-- @
-- import ATProto.Repo.ListRecords
-- import ATProto.XRPC.Http
--
-- main :: IO ()
-- main = do
--   client <- newHttpXrpcClient "https://bsky.social"
--   result <- listRecords client $
--     defaultListRecordsParams "haileyok.com" "app.bsky.feed.post"
--   case result of
--     Left  err  -> print err
--     Right resp -> do
--       mapM_ (print . rrUri) (lrrRecords resp)
--       print (lrrCursor resp)
-- @
listRecords
  :: XrpcClient c
  => c
  -> ListRecordsParams
  -> IO (Either XrpcError ListRecordsResponse)
listRecords client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.repo.listRecords"
    , xrpcReqParams  = toQueryParams params
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

-- | Decode the JSON response body into a 'ListRecordsResponse'.
parseResponse :: BL.ByteString -> Either XrpcError ListRecordsResponse
parseResponse body =
  case LexJson.decode listRecordsResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
