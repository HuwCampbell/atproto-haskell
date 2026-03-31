-- | Typed binding for @com.atproto.sync.listRepos@.
--
-- Enumerates all the DID, rev, and commit CID for all repos hosted by this
-- service.  Does not require auth; implemented by PDS and Relay.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/sync/listRepos.json>.
module ATProto.Sync.ListRepos
  ( -- * Request parameters
    ListReposParams (..)
  , defaultListReposParams
    -- * Response
  , ListReposResponse (..)
  , ListReposRepo (..)
    -- * Codecs
  , listReposResponseCodec
  , listReposRepoCodec
    -- * Client function
  , listRepos
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

-- | Query parameters for @com.atproto.sync.listRepos@.
data ListReposParams = ListReposParams
  { lrpLimit  :: Maybe Int
    -- ^ Maximum number of repos to return (1–1000, default 500).
  , lrpCursor :: Maybe T.Text
    -- ^ Pagination cursor from a previous response.
  } deriving (Eq, Show)

-- | Default 'ListReposParams' with no pagination.
defaultListReposParams :: ListReposParams
defaultListReposParams = ListReposParams
  { lrpLimit  = Nothing
  , lrpCursor = Nothing
  }

-- | Convert 'ListReposParams' to XRPC query-string parameters.
toQueryParams :: ListReposParams -> Map.Map T.Text T.Text
toQueryParams p =
  Map.fromList $ Maybe.catMaybes
    [ (\n -> ("limit",  T.pack (show n))) <$> lrpLimit p
    , (\c -> ("cursor", c))               <$> lrpCursor p
    ]

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | A single repo entry in the @listRepos@ response.
--
-- Corresponds to @com.atproto.sync.listRepos#repo@.
data ListReposRepo = ListReposRepo
  { lrrDid    :: T.Text
    -- ^ The DID of the repo.
  , lrrHead   :: T.Text
    -- ^ Current repo commit CID.
  , lrrRev    :: T.Text
    -- ^ Current revision TID.
  , lrrActive :: Maybe Bool
    -- ^ Whether the repo is active.
  , lrrStatus :: Maybe T.Text
    -- ^ If active=false, possible reason.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.sync.listRepos#repo@.
listReposRepoCodec :: Codec ListReposRepo
listReposRepoCodec =
    Codec.record "com.atproto.sync.listRepos#repo" $
        ListReposRepo
            <$> Codec.requiredField "did"    Codec.did                           lrrDid
            <*> Codec.requiredField "head"   (Codec.string Codec.LexFormatCid)   lrrHead
            <*> Codec.requiredField "rev"    (Codec.string Codec.LexFormatTid)   lrrRev
            <*> Codec.optionalField "active" Codec.bool                          lrrActive
            <*> Codec.optionalField "status" Codec.text                          lrrStatus

-- | Response from @com.atproto.sync.listRepos@.
data ListReposResponse = ListReposResponse
  { lrsrCursor :: Maybe T.Text
    -- ^ Pagination cursor; present when more repos are available.
  , lrsrRepos  :: [ListReposRepo]
    -- ^ The listed repos.
  } deriving (Eq, Show)

-- | Codec for the @listRepos@ response body.
listReposResponseCodec :: Codec ListReposResponse
listReposResponseCodec =
    Codec.record "com.atproto.sync.listRepos#response" $
        ListReposResponse
            <$> Codec.optionalField "cursor" Codec.text                        lrsrCursor
            <*> Codec.requiredField "repos"  (Codec.array listReposRepoCodec)  lrsrRepos

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.sync.listRepos@ using the given XRPC client.
listRepos
  :: XrpcClient c
  => c
  -> ListReposParams
  -> IO (Either XrpcError ListReposResponse)
listRepos client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.sync.listRepos"
    , xrpcReqParams  = toQueryParams params
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError ListReposResponse
parseResponse body =
  case LexJson.decode listReposResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
