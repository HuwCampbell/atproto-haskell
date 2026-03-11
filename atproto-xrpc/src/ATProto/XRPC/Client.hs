-- | XRPC client interface.
--
-- Defines the 'XrpcClient' typeclass that any concrete HTTP backend must
-- implement, along with helper combinators for building query and procedure
-- calls.
module ATProto.XRPC.Client
  ( -- * Client typeclass
    XrpcClient (..)
    -- * Helpers
  , xrpcQuery
  , xrpcProcedure
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.XRPC.Types

-- | A typeclass for backends that can execute XRPC requests.
--
-- Minimal complete definition: 'runXrpc'.
class XrpcClient c where
  -- | Execute a prepared 'XrpcRequest' and return either an 'XrpcError' or
  -- a successful 'XrpcResponse'.
  runXrpc :: c -> XrpcRequest -> IO (Either XrpcError XrpcResponse)

-- | Build and execute an XRPC query (HTTP GET).
xrpcQuery
  :: XrpcClient c
  => c
  -> T.Text                   -- ^ NSID
  -> Map.Map T.Text T.Text    -- ^ Query parameters
  -> IO (Either XrpcError XrpcResponse)
xrpcQuery client nsid params =
  runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = nsid
    , xrpcReqParams  = params
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }

-- | Build and execute an XRPC procedure (HTTP POST).
xrpcProcedure
  :: XrpcClient c
  => c
  -> T.Text                   -- ^ NSID
  -> Map.Map T.Text T.Text    -- ^ Query parameters
  -> Maybe BL.ByteString      -- ^ Request body
  -> IO (Either XrpcError XrpcResponse)
xrpcProcedure client nsid params body =
  runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = nsid
    , xrpcReqParams  = params
    , xrpcReqBody    = body
    , xrpcReqHeaders = Map.empty
    }
