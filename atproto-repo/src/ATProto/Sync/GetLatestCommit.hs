-- | Typed binding for @com.atproto.sync.getLatestCommit@.
--
-- Get the current commit CID & revision of the specified repo.
-- Does not require auth.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/sync/getLatestCommit.json>.
module ATProto.Sync.GetLatestCommit
  ( -- * Request parameters
    GetLatestCommitParams (..)
    -- * Response
  , GetLatestCommitResponse (..)
    -- * Codecs
  , getLatestCommitResponseCodec
    -- * Client function
  , getLatestCommit
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

-- | Query parameters for @com.atproto.sync.getLatestCommit@.
newtype GetLatestCommitParams = GetLatestCommitParams
  { glcpDid :: T.Text
    -- ^ The DID of the repo.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.sync.getLatestCommit@.
data GetLatestCommitResponse = GetLatestCommitResponse
  { glcrCid :: T.Text
    -- ^ The commit CID.
  , glcrRev :: T.Text
    -- ^ The revision TID.
  } deriving (Eq, Show)

-- | Codec for the @getLatestCommit@ response body.
getLatestCommitResponseCodec :: Codec GetLatestCommitResponse
getLatestCommitResponseCodec =
    Codec.record "com.atproto.sync.getLatestCommit#response" $
        GetLatestCommitResponse
            <$> Codec.requiredField "cid" (Codec.string Codec.LexFormatCid) glcrCid
            <*> Codec.requiredField "rev" (Codec.string Codec.LexFormatTid) glcrRev

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.sync.getLatestCommit@ using the given XRPC client.
getLatestCommit
  :: XrpcClient c
  => c
  -> GetLatestCommitParams
  -> IO (Either XrpcError GetLatestCommitResponse)
getLatestCommit client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.sync.getLatestCommit"
    , xrpcReqParams  = Map.singleton "did" (glcpDid params)
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError GetLatestCommitResponse
parseResponse body =
  case LexJson.decode getLatestCommitResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
