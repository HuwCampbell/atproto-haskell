-- | AT Protocol XRPC server framework.
--
-- This module provides the top-level API for building an XRPC server as a
-- WAI 'Network.Wai.Middleware'.  Handlers are parametric in the application
-- monad @m@, which may be any 'Control.Monad.IO.Class.MonadIO' stack.
--
-- == Quick start: plain 'IO'
--
-- For simple applications that run directly in 'IO', pass 'id' as the
-- runner:
--
-- @
-- import ATProto.XRPC.Server
-- import ATProto.XRPC.Server.Wai   (xrpcMiddleware)
-- import ATProto.Syntax             (parseNSID)
-- import Network.Wai.Handler.Warp   (run)
--
-- myHandler :: XrpcServerRequest -> IO XrpcHandlerResult
-- myHandler _req = return (XrpcSuccess \"{}\")
--
-- main :: IO ()
-- main = do
--   let Right nsid = parseNSID \"com.example.ping\"
--       server     = makeServer [ query nsid myHandler ]
--   run 3000 (xrpcMiddleware id server (\\_ respond -> respond notFound))
-- @
--
-- == Using a 'Control.Monad.Trans.Reader.ReaderT' application monad
--
-- In a real application you will typically thread an environment through
-- every handler.  Define an application monad, write handlers in it, and
-- supply a runner /once/ at the WAI boundary:
--
-- @
-- import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
-- import ATProto.XRPC.Server
-- import ATProto.XRPC.Server.Wai   (xrpcMiddleware)
-- import ATProto.Syntax             (parseNSID)
-- import Network.Wai.Handler.Warp   (run)
--
-- data AppEnv = AppEnv
--   { envConn     :: Connection
--   , envSessions :: SessionMap
--   }
--
-- type AppM = ReaderT AppEnv IO
--
-- handleGetStatuses :: XrpcServerRequest -> AppM XrpcHandlerResult
-- handleGetStatuses _req = do
--   conn <- asks envConn
--   rows <- liftIO (fetchRows conn)
--   return (XrpcSuccess (encodeRows rows))
--
-- handleSetStatus :: XrpcServerRequest -> AppM XrpcHandlerResult
-- handleSetStatus req = do
--   case xsrBody req of
--     Nothing  -> return (XrpcHandlerError \"InvalidRequest\" (Just \"body required\"))
--     Just _bs -> return XrpcAccepted
--
-- xrpcServer :: XrpcServer AppM
-- xrpcServer =
--   let Right getStatuses = parseNSID \"xyz.statusphere.getStatuses\"
--       Right setStatus   = parseNSID \"xyz.statusphere.setStatus\"
--   in makeServer
--       [ query     getStatuses handleGetStatuses
--       , procedure setStatus   handleSetStatus
--       ]
--
-- main :: IO ()
-- main = do
--   env <- buildEnv
--   run 3001 (xrpcMiddleware (flip runReaderT env) xrpcServer fallbackApp)
-- @
--
-- == Adding authentication
--
-- Attach an 'AuthVerifier' to the server with 'withAuthVerifier'.  The
-- verifier runs before every handler dispatch; on success the caller\'s
-- DID (if any) is available as 'xsrCaller'.
--
-- === Service-to-service JWT authentication
--
-- Use 'ATProto.ServiceAuth.verifyServiceJwt' from @atproto-haskell-service-auth@:
--
-- @
-- import qualified Data.CaseInsensitive       as CI
-- import qualified Data.Map.Strict            as Map
-- import           ATProto.ServiceAuth        (ServiceAuthError, verifyServiceJwt)
-- import           ATProto.XRPC.Server
--
-- -- Build an AuthVerifier that validates service-auth JWTs.
-- -- @myDid@ is this server\'s own DID (the expected audience).
-- -- @lookupKey iss refresh@ returns the public key for the issuer DID.
-- serviceAuthVerifier :: Text -> (Text -> Bool -> IO (Either String PubKey)) -> AuthVerifier IO
-- serviceAuthVerifier myDid lookupKey headers =
--   case Map.lookup (CI.mk \"authorization\") headers of
--     Nothing ->
--       return (AuthFailed \"AuthRequired\" (Just \"Authorization header missing\"))
--     Just authVal ->
--       case T.stripPrefix \"Bearer \" authVal of
--         Nothing ->
--           return (AuthFailed \"AuthRequired\" (Just \"Expected Bearer token\"))
--         Just jwt -> do
--           result <- verifyServiceJwt jwt (Just myDid) Nothing lookupKey
--           return \$ case result of
--             Left err  -> AuthFailed \"AuthRequired\" (Just (T.pack (show err)))
--             Right pay -> AuthOk (Just (payloadIss pay))
--
-- -- Attach to the server
-- secureServer :: XrpcServer IO
-- secureServer =
--   withAuthVerifier (serviceAuthVerifier myDid lookupKey)
--     (makeServer [ query nsid myHandler ])
-- @
--
-- === Reading the caller DID in a handler
--
-- @
-- protectedHandler :: XrpcServerRequest -> AppM XrpcHandlerResult
-- protectedHandler req =
--   case xsrCaller req of
--     Nothing  -> return (XrpcHandlerError \"AuthRequired\" (Just \"Login required\"))
--     Just did -> do
--       -- use did as the authenticated caller\'s DID
--       return (XrpcSuccess (encode did))
-- @
module ATProto.XRPC.Server
  ( -- * Smart constructors
    query
  , procedure
  , makeServer
  , withAuthVerifier
    -- * Re-exports from 'ATProto.XRPC.Server.Types'
  , XrpcServerRequest (..)
  , XrpcHandlerResult (..)
  , XrpcHandler
  , AuthResult (..)
  , AuthVerifier
  , XrpcEndpoint (..)
  , XrpcServer (..)
    -- * Re-exports from 'ATProto.XRPC.Server.Handler'
  , Handler
  , runHandler
  , requireAuth
  , requireBody
  , decodeBody
  , requireParam
  , throwXrpc
  , respondCodec
  , respondAccepted
  , respondRaw
  , liftAction
  ) where

import qualified Data.Map.Strict as Map

import ATProto.Syntax.NSID       (NSID)
import ATProto.XRPC.Types        (XrpcMethod (..))
import ATProto.XRPC.Server.Types
import ATProto.XRPC.Server.Handler

-- | Register a query (HTTP GET) endpoint.
query :: NSID -> XrpcHandler m did -> XrpcEndpoint m did
query nsid = XrpcEndpoint nsid XrpcQuery

-- | Register a procedure (HTTP POST) endpoint.
procedure :: NSID -> XrpcHandler m did -> XrpcEndpoint m did
procedure nsid = XrpcEndpoint nsid XrpcProcedure

-- | Build an 'XrpcServer' from a list of endpoints, with no auth verifier.
--
-- All endpoints are publicly accessible by default.  Use 'withAuthVerifier'
-- to add authentication.
--
-- If two endpoints share the same NSID and method, the last one wins.
makeServer :: [XrpcEndpoint m did] -> XrpcServer m did
makeServer eps = XrpcServer
  { xsEndpoints    = Map.fromList [ ((xeMethod ep, xeNsid ep), ep) | ep <- eps ]
  , xsAuthVerifier = Nothing
  }

-- | Attach an 'AuthVerifier' to an 'XrpcServer'.
--
-- The verifier will be run on every request before the handler is
-- dispatched.  On 'AuthFailed' the middleware returns HTTP 401 with an
-- XRPC error body and the handler is never called.  On 'AuthOk' the
-- caller\'s DID (if any) is injected into 'xsrCaller'.
--
-- @
-- secureServer :: XrpcServer IO
-- secureServer = withAuthVerifier myVerifier (makeServer myEndpoints)
-- @
withAuthVerifier :: AuthVerifier m did -> XrpcServer m did -> XrpcServer m did
withAuthVerifier v srv = srv { xsAuthVerifier = Just v }
