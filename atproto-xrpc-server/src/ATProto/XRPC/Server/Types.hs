-- | Core types for the XRPC server framework.
module ATProto.XRPC.Server.Types
  ( -- * Request
    XrpcServerRequest (..)
    -- * Result
  , XrpcHandlerResult (..)
    -- * Handler
  , XrpcHandler
    -- * Endpoint
  , XrpcEndpoint (..)
    -- * Server
  , XrpcServer (..)
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.Syntax.NSID  (NSID)
import ATProto.XRPC.Types   (XrpcHeaders, XrpcMethod)

-- | A parsed, routable XRPC server request.
data XrpcServerRequest = XrpcServerRequest
  { xsrNsid    :: NSID
    -- ^ Validated NSID extracted from the request path.
  , xsrParams  :: Map.Map T.Text T.Text
    -- ^ Decoded query-string parameters.
  , xsrBody    :: Maybe BL.ByteString
    -- ^ Raw request body (present for procedures, absent for queries).
  , xsrHeaders :: XrpcHeaders
    -- ^ Request headers as a case-insensitive map.
  }

-- | What a handler returns.
data XrpcHandlerResult
  = XrpcSuccess BL.ByteString
    -- ^ Successful response: HTTP 200 with the supplied body.
  | XrpcAccepted
    -- ^ Accepted: HTTP 202, no body.
  | XrpcHandlerError T.Text (Maybe T.Text)
    -- ^ Handler-level error serialised as @{\"error\":\"...\",\"message\":\"...\"}@.
    -- Returns HTTP 400.  The first field is the machine-readable error token;
    -- the second is an optional human-readable message.

-- | A handler for a single XRPC endpoint.
--
-- The handler runs in the application monad @m@.  Use 'Control.Monad.IO.Class.liftIO'
-- to perform IO operations.
type XrpcHandler m = XrpcServerRequest -> m XrpcHandlerResult

-- | Registration of one XRPC endpoint.
data XrpcEndpoint m = XrpcEndpoint
  { xeNsid    :: NSID
    -- ^ The NSID this endpoint handles.
  , xeMethod  :: XrpcMethod
    -- ^ Whether this is a query (GET) or procedure (POST).
  , xeHandler :: XrpcHandler m
    -- ^ The handler function.
  }

-- | A collection of registered XRPC endpoints, keyed by method and NSID.
newtype XrpcServer m = XrpcServer
  { xsEndpoints :: Map.Map (XrpcMethod, NSID) (XrpcEndpoint m)
    -- ^ Internal routing table.  Build with 'ATProto.XRPC.Server.makeServer'.
  }
