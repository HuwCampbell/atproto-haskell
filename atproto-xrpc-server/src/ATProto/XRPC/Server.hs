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
-- -- Application environment
-- data AppEnv = AppEnv
--   { envConn     :: Connection
--   , envSessions :: SessionMap
--   }
--
-- type AppM = ReaderT AppEnv IO
--
-- -- Handler written naturally against the env
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
-- -- Build the server once
-- xrpcServer :: XrpcServer AppM
-- xrpcServer =
--   let Right getStatuses = parseNSID \"xyz.statusphere.getStatuses\"
--       Right setStatus   = parseNSID \"xyz.statusphere.setStatus\"
--   in makeServer
--       [ query     getStatuses handleGetStatuses
--       , procedure setStatus   handleSetStatus
--       ]
--
-- -- Wire up: runner is supplied once in main
-- main :: IO ()
-- main = do
--   env <- buildEnv
--   run 3001 (xrpcMiddleware (flip runReaderT env) xrpcServer fallbackApp)
-- @
module ATProto.XRPC.Server
  ( -- * Smart constructors
    query
  , procedure
  , makeServer
    -- * Re-exports from 'ATProto.XRPC.Server.Types'
  , XrpcServerRequest (..)
  , XrpcHandlerResult (..)
  , XrpcHandler
  , XrpcEndpoint (..)
  , XrpcServer (..)
  ) where

import qualified Data.Map.Strict as Map

import ATProto.Syntax.NSID       (NSID)
import ATProto.XRPC.Types        (XrpcMethod (..))
import ATProto.XRPC.Server.Types

-- | Register a query (HTTP GET) endpoint.
query :: NSID -> XrpcHandler m -> XrpcEndpoint m
query nsid handler = XrpcEndpoint nsid XrpcQuery handler

-- | Register a procedure (HTTP POST) endpoint.
procedure :: NSID -> XrpcHandler m -> XrpcEndpoint m
procedure nsid handler = XrpcEndpoint nsid XrpcProcedure handler

-- | Build an 'XrpcServer' from a list of endpoints.
--
-- If two endpoints share the same NSID and method, the last one wins.
makeServer :: [XrpcEndpoint m] -> XrpcServer m
makeServer eps = XrpcServer $ Map.fromList
  [ ((xeMethod ep, xeNsid ep), ep) | ep <- eps ]
