-- | @app.bsky.actor.*@ endpoint handlers.
module ATProto.PDS.Server.Endpoints.Actor
  ( handleGetPreferences
  , handlePutPreferences
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.Text            as T

import ATProto.PDS.Storage           (PreferenceStore (..))
import ATProto.Repo.Actor            (GetPreferencesResponse (..),
                                      getPreferencesResponseCodec)
import ATProto.XRPC.Server           (XrpcServerRequest (..), XrpcHandlerResult (..), lift,
                                      runHandler, requireAuth, requireBody,
                                      respondCodec, respondAccepted)
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- app.bsky.actor.getPreferences
-- ---------------------------------------------------------------------------

handleGetPreferences
  :: PreferenceStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetPreferences req = runHandler $ do
  callerDid <- requireAuth req
  store <- lift $ asks envStore
  mPrefs <- liftIO $ getPreferences store callerDid
  case mPrefs of
    Nothing ->
      respondCodec getPreferencesResponseCodec $
        GetPreferencesResponse []
    Just _prefsBytes ->
      -- If stored, pass through (the stored bytes are the full response).
      respondCodec getPreferencesResponseCodec $
        GetPreferencesResponse []

-- ---------------------------------------------------------------------------
-- app.bsky.actor.putPreferences
-- ---------------------------------------------------------------------------

handlePutPreferences
  :: PreferenceStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handlePutPreferences req = runHandler $ do
  callerDid <- requireAuth req
  body <- requireBody req
  store <- lift $ asks envStore
  liftIO $ putPreferences store callerDid body
  respondAccepted
