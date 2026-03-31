-- | Typed binding for @com.atproto.server.deleteSession@.
--
-- Delete the current session.  Requires auth using the @refreshJwt@
-- (not the @accessJwt@).  The method has no request body and no response
-- body on success.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/server/deleteSession.json>.
module ATProto.Server.DeleteSession
  ( -- * Client function
    deleteSession
  ) where

import qualified Data.Map.Strict         as Map

import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..))

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.server.deleteSession@ using the given XRPC client.
--
-- Requires auth using the refresh JWT.  Returns 'Right ()' on success.
deleteSession
  :: XrpcClient c
  => c
  -> IO (Either XrpcError ())
deleteSession client = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.server.deleteSession"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err -> return (Left err)
    Right _   -> return (Right ())
