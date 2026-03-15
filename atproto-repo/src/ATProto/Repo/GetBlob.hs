-- | Typed binding for @com.atproto.sync.getBlob@.
--
-- Fetch the raw bytes of a blob from a PDS or relay.
-- The Lexicon is at:
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/sync/getBlob.json>
module ATProto.Repo.GetBlob
  ( -- * Request parameters
    GetBlobParams (..)
    -- * Client function
  , getBlob
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.XRPC.Client (XrpcClient (..))
import           ATProto.XRPC.Types  (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request parameters
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.sync.getBlob@.
data GetBlobParams = GetBlobParams
  { gbpDid :: T.Text
    -- ^ The DID of the repository that owns the blob.
  , gbpCid :: T.Text
    -- ^ The CID of the blob to fetch.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Fetch the raw bytes of a blob from a PDS or relay.
--
-- Executes @com.atproto.sync.getBlob@ as an XRPC query (HTTP GET) and
-- returns the raw response body on success.
getBlob
  :: XrpcClient c
  => c
  -> GetBlobParams
  -> IO (Either XrpcError BL.ByteString)
getBlob client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.sync.getBlob"
    , xrpcReqParams  = Map.fromList
        [ ("did", gbpDid params)
        , ("cid", gbpCid params)
        ]
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (Right (xrpcRespBody resp))
