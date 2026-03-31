-- | Typed binding for @com.atproto.sync.requestCrawl@.
--
-- Request a service to persistently crawl hosted repos.  Expected use is
-- new PDS instances declaring their existence to Relays.  Does not require
-- auth.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/sync/requestCrawl.json>.
module ATProto.Sync.RequestCrawl
  ( -- * Request
    RequestCrawlRequest (..)
    -- * Codecs
  , requestCrawlRequestCodec
    -- * Client function
  , requestCrawl
  ) where

import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..))

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Input body for @com.atproto.sync.requestCrawl@.
newtype RequestCrawlRequest = RequestCrawlRequest
  { rcrHostname :: T.Text
    -- ^ Hostname of the current service (eg, PDS) that is requesting to be
    -- crawled.
  } deriving (Eq, Show)

-- | Codec for the @requestCrawl@ request body.
requestCrawlRequestCodec :: Codec RequestCrawlRequest
requestCrawlRequestCodec =
    Codec.record "com.atproto.sync.requestCrawl#request" $
        RequestCrawlRequest
            <$> Codec.requiredField "hostname" Codec.text rcrHostname

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.sync.requestCrawl@ using the given XRPC client.
--
-- Returns 'Right ()' on success (no response body).
requestCrawl
  :: XrpcClient c
  => c
  -> RequestCrawlRequest
  -> IO (Either XrpcError ())
requestCrawl client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.sync.requestCrawl"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (LexJson.encode requestCrawlRequestCodec req)
    , xrpcReqHeaders = Map.singleton "Content-Type" "application/json"
    }
  case result of
    Left  err -> return (Left err)
    Right _   -> return (Right ())
