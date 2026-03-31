-- | Typed binding for @com.atproto.identity.resolveHandle@.
--
-- Resolves an atproto handle (hostname) to a DID.  Does not necessarily
-- bi-directionally verify against the DID document.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/identity/resolveHandle.json>.
module ATProto.Identity.ResolveHandle
  ( -- * Request parameters
    ResolveHandleParams (..)
    -- * Response
  , ResolveHandleResponse (..)
    -- * Codecs
  , resolveHandleResponseCodec
    -- * Client function
  , resolveHandle
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request parameters
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.identity.resolveHandle@.
newtype ResolveHandleParams = ResolveHandleParams
  { rhpHandle :: T.Text
    -- ^ The handle to resolve.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.identity.resolveHandle@.
newtype ResolveHandleResponse = ResolveHandleResponse
  { rhrDid :: T.Text
    -- ^ The resolved DID.
  } deriving (Eq, Show)

-- | Codec for the @resolveHandle@ response body.
resolveHandleResponseCodec :: Codec ResolveHandleResponse
resolveHandleResponseCodec =
    Codec.record "com.atproto.identity.resolveHandle#response" $
        ResolveHandleResponse
            <$> Codec.requiredField "did" Codec.did rhrDid

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.identity.resolveHandle@ using the given XRPC client.
resolveHandle
  :: XrpcClient c
  => c
  -> ResolveHandleParams
  -> IO (Either XrpcError ResolveHandleResponse)
resolveHandle client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.identity.resolveHandle"
    , xrpcReqParams  = Map.singleton "handle" (rhpHandle params)
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError ResolveHandleResponse
parseResponse body =
  case LexJson.decode resolveHandleResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
