-- | Typed binding for @com.atproto.sync.getRepo@.
--
-- Download a repository export as CAR file.  Optionally only a \'diff\'
-- since a previous revision.  Does not require auth; implemented by PDS.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/sync/getRepo.json>.
module ATProto.Sync.GetRepo
  ( -- * Request parameters
    GetRepoParams (..)
    -- * Client function
  , getRepo
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Maybe              as Maybe
import qualified Data.Text               as T

import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request parameters
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.sync.getRepo@.
data GetRepoParams = GetRepoParams
  { grpDid   :: T.Text
    -- ^ The DID of the repo.
  , grpSince :: Maybe T.Text
    -- ^ The revision (@rev@) of the repo to create a diff from.
  } deriving (Eq, Show)

-- | Convert 'GetRepoParams' to XRPC query-string parameters.
toQueryParams :: GetRepoParams -> Map.Map T.Text T.Text
toQueryParams p =
  Map.fromList $
    [ ("did", grpDid p)
    ] ++ Maybe.catMaybes
    [ (\s -> ("since", s)) <$> grpSince p
    ]

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Fetch a repository as a CAR file.
--
-- Executes @com.atproto.sync.getRepo@ as an XRPC query (HTTP GET) and
-- returns the raw CAR bytes on success.
getRepo
  :: XrpcClient c
  => c
  -> GetRepoParams
  -> IO (Either XrpcError BL.ByteString)
getRepo client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.sync.getRepo"
    , xrpcReqParams  = toQueryParams params
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (Right (xrpcRespBody resp))
