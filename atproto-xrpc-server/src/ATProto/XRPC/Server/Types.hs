-- | Core types for the XRPC server framework.
module ATProto.XRPC.Server.Types
  ( -- * Request
    XrpcServerRequest (..)
    -- * Result
  , XrpcHandlerResult (..)
    -- * Handler
  , XrpcHandler
    -- * Authentication
  , AuthResult (..)
  , AuthVerifier
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
  , xsrCaller  :: Maybe T.Text
    -- ^ DID of the authenticated caller, populated by the server's
    -- 'AuthVerifier' if one is configured.  'Nothing' means either no
    -- verifier is configured or the verifier indicated anonymous access.
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

-- | The result of running an 'AuthVerifier'.
data AuthResult
  = AuthOk (Maybe T.Text)
    -- ^ Authentication succeeded (or the endpoint is public).
    -- The 'T.Text' value, when present, is the caller's DID.
    -- 'Nothing' indicates anonymous access; the request will proceed
    -- with 'xsrCaller' set to 'Nothing'.
  | AuthFailed T.Text (Maybe T.Text)
    -- ^ Authentication failed.  The middleware returns HTTP 401 with
    -- @{\"error\":\"...\",\"message\":\"...\"}@ and the handler is never called.
    -- The first field is the machine-readable error token
    -- (e.g. @\"AuthRequired\"@); the second is an optional human-readable
    -- message.

-- | An authentication verifier for an XRPC server.
--
-- Receives the request headers (including the @Authorization@ header)
-- and returns an 'AuthResult'.  The verifier runs in the application
-- monad @m@, so it can read environment values, call IO, or consult a
-- key cache.
--
-- Plug in any authentication scheme:
--
-- * Service-to-service JWTs via 'ATProto.ServiceAuth.verifyServiceJwt'
-- * OAuth DPoP bearer tokens via 'ATProto.OAuth.Client'
-- * A custom @Basic@ / API-key verifier for admin endpoints
--
-- See 'ATProto.XRPC.Server.withAuthVerifier' for how to attach a
-- verifier to an 'XrpcServer'.
type AuthVerifier m = XrpcHeaders -> m AuthResult

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
data XrpcServer m = XrpcServer
  { xsEndpoints    :: Map.Map (XrpcMethod, NSID) (XrpcEndpoint m)
    -- ^ Internal routing table.  Build with 'ATProto.XRPC.Server.makeServer'.
  , xsAuthVerifier :: Maybe (AuthVerifier m)
    -- ^ Optional authentication verifier.  When present, the middleware
    -- runs it on every request before calling the handler.  On 'AuthFailed'
    -- the middleware returns HTTP 401 immediately.  On 'AuthOk' the caller
    -- DID (if any) is injected into 'xsrCaller'.
    -- Attach one with 'ATProto.XRPC.Server.withAuthVerifier'.
  }
