-- | Typed binding for @com.atproto.server.getServiceAuth@.
--
-- Get a signed token on behalf of the requesting DID for the requested
-- service.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/server/getServiceAuth.json>.
module ATProto.Server.GetServiceAuth
  ( -- * Request parameters
    GetServiceAuthParams (..)
    -- * Response
  , GetServiceAuthResponse (..)
    -- * Codecs
  , getServiceAuthResponseCodec
    -- * Client function
  , getServiceAuth
  ) where

import           Data.Int                (Int64)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Maybe              as Maybe
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request parameters
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.server.getServiceAuth@.
data GetServiceAuthParams = GetServiceAuthParams
  { gsapAud :: T.Text
    -- ^ The DID of the service that the token will be used to authenticate with.
  , gsapExp :: Maybe Int64
    -- ^ The time in Unix Epoch seconds that the JWT expires.
  , gsapLxm :: Maybe T.Text
    -- ^ Lexicon (XRPC) method to bind the requested token to.
  } deriving (Eq, Show)

-- | Convert 'GetServiceAuthParams' to XRPC query-string parameters.
toQueryParams :: GetServiceAuthParams -> Map.Map T.Text T.Text
toQueryParams p =
  Map.fromList $
    [ ("aud", gsapAud p)
    ] ++ Maybe.catMaybes
    [ (\n -> ("exp", T.pack (show n))) <$> gsapExp p
    , (\l -> ("lxm", l))               <$> gsapLxm p
    ]

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.server.getServiceAuth@.
newtype GetServiceAuthResponse = GetServiceAuthResponse
  { gsaresToken :: T.Text
    -- ^ The signed service auth token.
  } deriving (Eq, Show)

-- | Codec for the @getServiceAuth@ response body.
getServiceAuthResponseCodec :: Codec GetServiceAuthResponse
getServiceAuthResponseCodec =
    Codec.record "com.atproto.server.getServiceAuth#response" $
        GetServiceAuthResponse
            <$> Codec.requiredField "token" Codec.text gsaresToken

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.server.getServiceAuth@ using the given XRPC client.
getServiceAuth
  :: XrpcClient c
  => c
  -> GetServiceAuthParams
  -> IO (Either XrpcError GetServiceAuthResponse)
getServiceAuth client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.server.getServiceAuth"
    , xrpcReqParams  = toQueryParams params
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError GetServiceAuthResponse
parseResponse body =
  case LexJson.decode getServiceAuthResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
