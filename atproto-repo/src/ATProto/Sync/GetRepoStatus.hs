-- | Typed binding for @com.atproto.sync.getRepoStatus@.
--
-- Get the hosting status for a repository, on this server.  Expected to be
-- implemented by PDS and Relay.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/sync/getRepoStatus.json>.
module ATProto.Sync.GetRepoStatus
  ( -- * Request parameters
    GetRepoStatusParams (..)
    -- * Response
  , GetRepoStatusResponse (..)
    -- * Codecs
  , getRepoStatusResponseCodec
    -- * Client function
  , getRepoStatus
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
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

-- | Query parameters for @com.atproto.sync.getRepoStatus@.
newtype GetRepoStatusParams = GetRepoStatusParams
  { grspDid :: T.Text
    -- ^ The DID of the repo.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.sync.getRepoStatus@.
data GetRepoStatusResponse = GetRepoStatusResponse
  { grsrDid    :: T.Text
    -- ^ The DID of the repo.
  , grsrActive :: Bool
    -- ^ Whether the repo is active.
  , grsrStatus :: Maybe T.Text
    -- ^ If active=false, possible reason (takendown, suspended, deleted,
    -- deactivated, desynchronized, throttled).
  , grsrRev    :: Maybe T.Text
    -- ^ Current rev of the repo, if active=true.
  } deriving (Eq, Show)

-- | Codec for the @getRepoStatus@ response body.
getRepoStatusResponseCodec :: Codec GetRepoStatusResponse
getRepoStatusResponseCodec =
    Codec.record "com.atproto.sync.getRepoStatus#response" $
        GetRepoStatusResponse
            <$> Codec.requiredField "did"    Codec.did                           grsrDid
            <*> Codec.requiredField "active" Codec.bool                          grsrActive
            <*> Codec.optionalField "status" Codec.text                          grsrStatus
            <*> Codec.optionalField "rev"    (Codec.string Codec.LexFormatTid)   grsrRev

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.sync.getRepoStatus@ using the given XRPC client.
getRepoStatus
  :: XrpcClient c
  => c
  -> GetRepoStatusParams
  -> IO (Either XrpcError GetRepoStatusResponse)
getRepoStatus client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.sync.getRepoStatus"
    , xrpcReqParams  = Map.singleton "did" (grspDid params)
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError GetRepoStatusResponse
parseResponse body =
  case LexJson.decode getRepoStatusResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
