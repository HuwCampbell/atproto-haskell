-- | Typed binding for @com.atproto.identity.resolveDid@.
--
-- Resolves DID to DID document.  Does not bi-directionally verify handle.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/identity/resolveDid.json>.
module ATProto.Identity.ResolveDid
  ( -- * Request parameters
    ResolveDidParams (..)
    -- * Response
  , ResolveDidResponse (..)
    -- * Codecs
  , resolveDidResponseCodec
    -- * Client function
  , resolveDid
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

-- | Query parameters for @com.atproto.identity.resolveDid@.
newtype ResolveDidParams = ResolveDidParams
  { rdpDid :: T.Text
    -- ^ DID to resolve.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.identity.resolveDid@.
newtype ResolveDidResponse = ResolveDidResponse
  { rdrDidDoc :: LexValue
    -- ^ The complete DID document for the identity.
  } deriving (Eq, Show)

-- | Codec for the @resolveDid@ response body.
resolveDidResponseCodec :: Codec ResolveDidResponse
resolveDidResponseCodec =
    Codec.record "com.atproto.identity.resolveDid#response" $
        ResolveDidResponse
            <$> Codec.requiredField "didDoc" Codec.lexValue rdrDidDoc

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.identity.resolveDid@ using the given XRPC client.
resolveDid
  :: XrpcClient c
  => c
  -> ResolveDidParams
  -> IO (Either XrpcError ResolveDidResponse)
resolveDid client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.identity.resolveDid"
    , xrpcReqParams  = Map.singleton "did" (rdpDid params)
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError ResolveDidResponse
parseResponse body =
  case LexJson.decode resolveDidResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
