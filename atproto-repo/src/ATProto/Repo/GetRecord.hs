{-# LANGUAGE TupleSections #-}
-- | Typed binding for @com.atproto.repo.getRecord@.
--
-- This module provides Haskell types for the query parameters and JSON
-- response of the @com.atproto.repo.getRecord@ XRPC method, plus a
-- convenience function 'getRecord' that works with any 'XrpcClient'.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/getRecord.json>.
module ATProto.Repo.GetRecord
  ( -- * Request parameters
    GetRecordParams (..)
  , defaultGetRecordParams
  , toQueryParams
    -- * Response
  , GetRecordResponse (..)
  , getRecordResponseCodec
    -- * Client function
  , getRecord
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Maybe              as Maybe
import qualified Data.Text               as T

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client (XrpcClient (..))
import           ATProto.XRPC.Types  (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.repo.getRecord@.
--
-- 'repo' and 'collection' are required; all other fields are optional.
data GetRecordParams = GetRecordParams
  { grpRepo       :: T.Text
    -- ^ The DID or handle of the repository, e.g.
    -- @\"did:plc:ewvi7nxzyoun6zhhandbv25b\"@ or @\"haileyok.com\"@.
  , grpCollection :: T.Text
    -- ^ NSID of the record type, e.g. @\"app.bsky.feed.post\"@.
  , grpRKey       :: T.Text
    -- ^ Maximum number of records to return (1–100, default 50).
  , grpCid        :: Maybe T.Text
  } deriving (Eq, Show)

-- | A 'GetRecordParams' with sensible defaults (limit = 50, no pagination).
defaultGetRecordParams :: T.Text -> T.Text -> T.Text -> GetRecordParams
defaultGetRecordParams repo collection rKey = GetRecordParams
  { grpRepo       = repo
  , grpCollection = collection
  , grpRKey       = rKey
  , grpCid        = Nothing
  }

-- | Convert 'GetRecordParams' to XRPC query-string parameters.
toQueryParams :: GetRecordParams -> Map.Map T.Text T.Text
toQueryParams p =
  Map.fromList $
    [ ("repo",       grpRepo p)
    , ("collection", grpCollection p)
    , ("rkey",       grpRKey p)
    ] ++ Maybe.maybeToList (("cid",) <$> grpCid p)


-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | A single record returned by @getRecord@.
data GetRecordResponse = GetRecordResponse
  { grrUri   :: T.Text
    -- ^ AT-URI of the record, e.g.
    -- @\"at:\/\/did:plc:abc\/app.bsky.feed.post\/3k7bj\"@.
  , grrCid   :: T.Text
    -- ^ CID of the record's current version.
  , grrValue :: LexValue
    -- ^ The raw record value (type depends on the collection).
  } deriving (Eq, Show)


-- | Codec for the @getRecord@ response body.
getRecordResponseCodec :: Codec GetRecordResponse
getRecordResponseCodec =
    Codec.record "com.atproto.repo.getRecord#output" $
        GetRecordResponse
            <$> Codec.requiredField "uri"   Codec.atUri     grrUri
            <*> Codec.requiredField "cid"   Codec.text      grrCid
            <*> Codec.requiredField "value" Codec.lexValue  grrValue


-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.repo.getRecord@ using the given XRPC client.
--
-- Returns 'Right' with the parsed response on success, or 'Left' with an
-- 'XrpcError' on failure.  The response body is decoded from JSON; a parse
-- failure is reported as an 'XrpcError' with the token @\"ParseError\"@.
--
-- = Example
--
-- @
-- import ATProto.Repo.GetRecord
-- import ATProto.XRPC.Http
--
-- main :: IO ()
-- main = do
--   client <- newHttpXrpcClient "https://bsky.social"
--   result <- getRecord client $
--     defaultGetRecordParams "haileyok.com" "app.bsky.feed.post" "3mfn5eshps227"
--   case result of
--     Left  err  -> print err
--     Right resp -> do
--       print (grrUri resp)
-- @
getRecord
  :: XrpcClient c
  => c
  -> GetRecordParams
  -> IO (Either XrpcError GetRecordResponse)
getRecord client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.repo.getRecord"
    , xrpcReqParams  = toQueryParams params
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

-- | Decode the JSON response body into a 'GetRecordResponse'.
parseResponse :: BL.ByteString -> Either XrpcError GetRecordResponse
parseResponse body =
  case LexJson.decode getRecordResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
