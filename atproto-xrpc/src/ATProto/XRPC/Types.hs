-- | XRPC type definitions.
--
-- Port of the core type definitions from \@atproto\/xrpc.
module ATProto.XRPC.Types
  ( -- * Request / response
    XrpcMethod (..)
  , XrpcRequest (..)
  , XrpcResponse (..)
  , XrpcError (..)
    -- * Headers
  , XrpcHeaders
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

-- | HTTP method used by an XRPC call.
data XrpcMethod
  = XrpcQuery     -- ^ Query – HTTP GET
  | XrpcProcedure -- ^ Procedure – HTTP POST
  deriving (Eq, Ord, Show)

-- | HTTP headers as a simple 'Map'.
type XrpcHeaders = Map.Map T.Text T.Text

-- | A prepared XRPC request.
data XrpcRequest = XrpcRequest
  { xrpcReqMethod  :: XrpcMethod
  , xrpcReqNsid    :: T.Text
    -- ^ The NSID of the lexicon method, e.g. @\"app.bsky.feed.getTimeline\"@.
  , xrpcReqParams  :: Map.Map T.Text T.Text
    -- ^ Query-string parameters (for queries).
  , xrpcReqBody    :: Maybe BL.ByteString
    -- ^ Request body (for procedures).
  , xrpcReqHeaders :: XrpcHeaders
  } deriving (Eq, Show)

-- | A successful XRPC response.
data XrpcResponse = XrpcResponse
  { xrpcRespStatus  :: Int
  , xrpcRespHeaders :: XrpcHeaders
  , xrpcRespBody    :: BL.ByteString
  } deriving (Eq, Show)

-- | An XRPC error returned by the server.
data XrpcError = XrpcError
  { xrpcErrError   :: T.Text
    -- ^ Machine-readable error token, e.g. @\"InvalidToken\"@.
  , xrpcErrMessage :: Maybe T.Text
    -- ^ Optional human-readable message.
  , xrpcErrStatus  :: Int
    -- ^ HTTP status code.
  , xrpcErrHeaders :: XrpcHeaders
    -- ^ Response headers.  Useful for extracting the @DPoP-Nonce@ header
    -- when the server returns a @use_dpop_nonce@ error.
  } deriving (Eq, Show)
