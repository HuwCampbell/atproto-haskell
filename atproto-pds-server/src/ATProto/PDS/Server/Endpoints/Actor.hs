-- | @app.bsky.actor.*@ endpoint handlers.
module ATProto.PDS.Server.Endpoints.Actor
  ( handleGetPreferences
  , handlePutPreferences
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy as BL
import           Data.IORef                 (readIORef, modifyIORef')
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.XRPC.Server.Types     (XrpcServerRequest (..), XrpcHandlerResult (..))
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- app.bsky.actor.getPreferences
-- ---------------------------------------------------------------------------

handleGetPreferences
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetPreferences req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just callerDid -> do
      env <- asks id
      prefs <- liftIO $ readIORef (envPreferences env)
      let stored = Map.findWithDefault "{\"preferences\":[]}" callerDid prefs
      return $ XrpcSuccess stored

-- ---------------------------------------------------------------------------
-- app.bsky.actor.putPreferences
-- ---------------------------------------------------------------------------

handlePutPreferences
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handlePutPreferences req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just callerDid ->
      case xsrBody req of
        Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "Request body required")
        Just body -> do
          env <- asks id
          liftIO $ modifyIORef' (envPreferences env) (Map.insert callerDid body)
          return XrpcAccepted
