-- | Typed binding for @com.atproto.sync.notifyOfUpdate@.
--
-- Notify a crawling service of a recent update, and that crawling should
-- resume.  Intended use is after a gap between repo stream events caused the
-- crawling service to disconnect.  Does not require auth; implemented by
-- Relay.
--
-- __DEPRECATED:__ just use @com.atproto.sync.requestCrawl@.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/sync/notifyOfUpdate.json>.
module ATProto.Sync.NotifyOfUpdate
  ( -- * Request
    NotifyOfUpdateRequest (..)
    -- * Codecs
  , notifyOfUpdateRequestCodec
    -- * Client function
  , notifyOfUpdate
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

-- | Input body for @com.atproto.sync.notifyOfUpdate@.
newtype NotifyOfUpdateRequest = NotifyOfUpdateRequest
  { nourHostname :: T.Text
    -- ^ Hostname of the current service (usually a PDS) that is notifying
    -- of update.
  } deriving (Eq, Show)

-- | Codec for the @notifyOfUpdate@ request body.
notifyOfUpdateRequestCodec :: Codec NotifyOfUpdateRequest
notifyOfUpdateRequestCodec =
    Codec.record "com.atproto.sync.notifyOfUpdate#request" $
        NotifyOfUpdateRequest
            <$> Codec.requiredField "hostname" Codec.text nourHostname

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.sync.notifyOfUpdate@ using the given XRPC client.
--
-- Returns 'Right ()' on success (no response body).
notifyOfUpdate
  :: XrpcClient c
  => c
  -> NotifyOfUpdateRequest
  -> IO (Either XrpcError ())
notifyOfUpdate client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.sync.notifyOfUpdate"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (LexJson.encode notifyOfUpdateRequestCodec req)
    , xrpcReqHeaders = Map.singleton "Content-Type" "application/json"
    }
  case result of
    Left  err -> return (Left err)
    Right _   -> return (Right ())
