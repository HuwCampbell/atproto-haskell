-- | @com.atproto.identity.*@ endpoint handlers.
module ATProto.PDS.Server.Endpoints.Identity
  ( handleResolveHandle
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy as BL
import           Data.IORef                 (readIORef)
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import ATProto.Syntax.DID            (unDID)
import ATProto.XRPC.Server.Types     (XrpcServerRequest (..), XrpcHandlerResult (..))
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- com.atproto.identity.resolveHandle
-- ---------------------------------------------------------------------------

handleResolveHandle
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleResolveHandle req = do
  let mHandle = Map.lookup "handle" (xsrParams req)
  case mHandle of
    Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "handle parameter required")
    Just handle -> do
      env <- asks id
      idx <- liftIO $ readIORef (envHandleIndex env)
      case Map.lookup handle idx of
        Nothing -> return $ XrpcHandlerError "HandleNotFound"
          (Just ("Unable to resolve handle: " <> handle))
        Just did ->
          return $ XrpcSuccess $ BL.fromStrict $ TE.encodeUtf8 $
            "{\"did\":\"" <> unDID did <> "\"}"
