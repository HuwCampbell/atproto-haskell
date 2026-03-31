-- | Typed binding for @com.atproto.repo.describeRepo@.
--
-- Get information about an account and repository, including the list of
-- collections.  Does not require auth.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/describeRepo.json>.
module ATProto.Repo.DescribeRepo
  ( -- * Request parameters
    DescribeRepoParams (..)
    -- * Response
  , DescribeRepoResponse (..)
    -- * Codecs
  , describeRepoResponseCodec
    -- * Client function
  , describeRepo
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request parameters
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.repo.describeRepo@.
newtype DescribeRepoParams = DescribeRepoParams
  { drpRepo :: T.Text
    -- ^ The handle or DID of the repo.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.repo.describeRepo@.
data DescribeRepoResponse = DescribeRepoResponse
  { drrHandle          :: T.Text
    -- ^ The account handle.
  , drrDid             :: T.Text
    -- ^ The account DID.
  , drrDidDoc          :: LexValue
    -- ^ The complete DID document for this account.
  , drrCollections     :: [T.Text]
    -- ^ List of all the collections (NSIDs) for which this repo contains
    -- at least one record.
  , drrHandleIsCorrect :: Bool
    -- ^ Indicates if handle is currently valid (resolves bi-directionally).
  } deriving (Eq, Show)

-- | Codec for the @describeRepo@ response body.
describeRepoResponseCodec :: Codec DescribeRepoResponse
describeRepoResponseCodec =
    Codec.record "com.atproto.repo.describeRepo#response" $
        DescribeRepoResponse
            <$> Codec.requiredField "handle"          Codec.handle                drrHandle
            <*> Codec.requiredField "did"             Codec.did                   drrDid
            <*> Codec.requiredField "didDoc"          Codec.lexValue              drrDidDoc
            <*> Codec.requiredField "collections"     (Codec.array Codec.text)    drrCollections
            <*> Codec.requiredField "handleIsCorrect" Codec.bool                  drrHandleIsCorrect

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.repo.describeRepo@ using the given XRPC client.
describeRepo
  :: XrpcClient c
  => c
  -> DescribeRepoParams
  -> IO (Either XrpcError DescribeRepoResponse)
describeRepo client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.repo.describeRepo"
    , xrpcReqParams  = Map.singleton "repo" (drpRepo params)
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError DescribeRepoResponse
parseResponse body =
  case LexJson.decode describeRepoResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
