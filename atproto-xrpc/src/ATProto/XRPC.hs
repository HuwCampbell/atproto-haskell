-- | AT Protocol XRPC client interface.
--
-- XRPC is the HTTP-based RPC transport used by the AT Protocol.  Every API
-- call is identified by an NSID (namespaced identifier) and is either a
-- __query__ (HTTP GET, read-only) or a __procedure__ (HTTP POST, write).
--
-- This package defines only the abstract interface – 'XrpcClient' – and the
-- request\/response types.  A concrete implementation backed by
-- @http-client-tls@ is provided by @atproto-haskell-xrpc-http@.
--
-- = Typical usage
--
-- == Calling a query (HTTP GET)
--
-- @
-- import ATProto.XRPC
-- import qualified Data.Map.Strict as Map
--
-- -- Assuming @client :: c@ where @XrpcClient c@.
-- getTimeline :: XrpcClient c => c -> IO (Either XrpcError XrpcResponse)
-- getTimeline client =
--   xrpcQuery client "app.bsky.feed.getTimeline"
--     (Map.fromList [("limit", "50")])
-- @
--
-- == Calling a procedure (HTTP POST)
--
-- @
-- import ATProto.XRPC
-- import qualified Data.ByteString.Lazy.Char8 as BLC
-- import qualified Data.Map.Strict            as Map
--
-- createPost :: XrpcClient c => c -> IO (Either XrpcError XrpcResponse)
-- createPost client =
--   xrpcProcedure client "com.atproto.repo.createRecord"
--     Map.empty
--     (Just (BLC.pack "{\"collection\":\"app.bsky.feed.post\"}"))
-- @
--
-- == Attaching an authorisation token
--
-- Supply the bearer token via 'xrpcReqHeaders'; the concrete client forwards
-- every header in the request:
--
-- @
-- import ATProto.XRPC
-- import qualified Data.Map.Strict as Map
--
-- authedQuery :: XrpcClient c => c -> Text -> IO (Either XrpcError XrpcResponse)
-- authedQuery client jwt =
--   runXrpc client XrpcRequest
--     { xrpcReqMethod  = XrpcQuery
--     , xrpcReqNsid    = "app.bsky.actor.getProfile"
--     , xrpcReqParams  = Map.fromList [("actor", "bsky.social")]
--     , xrpcReqBody    = Nothing
--     , xrpcReqHeaders = Map.fromList [("Authorization", "Bearer " <> jwt)]
--     }
-- @
module ATProto.XRPC
  ( -- * Client typeclass
    XrpcClient (..)
    -- * Request and response types
  , XrpcMethod (..)
  , XrpcRequest (..)
  , XrpcResponse (..)
  , XrpcError (..)
  , XrpcHeaders
    -- * Convenience helpers
  , xrpcQuery
  , xrpcProcedure
  ) where

import ATProto.XRPC.Types
import ATProto.XRPC.Client
