-- | @chat.bsky.convo.*@ endpoint handlers (stubs).
--
-- Chat functionality is not implemented in this development server.
-- These endpoints return empty results so that clients which call
-- them do not receive 501 errors.
module ATProto.PDS.Server.Endpoints.Chat
  ( handleGetLog
  , handleListConvos
  ) where

import qualified Data.Text as T

import ATProto.XRPC.Server.Types (XrpcServerRequest (..), XrpcHandlerResult (..))
import ATProto.PDS.Server.Env    (AppM)

-- ---------------------------------------------------------------------------
-- chat.bsky.convo.getLog
-- ---------------------------------------------------------------------------

handleGetLog
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetLog req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just _  -> return $ XrpcSuccess "{\"logs\":[]}"

-- ---------------------------------------------------------------------------
-- chat.bsky.convo.listConvos
-- ---------------------------------------------------------------------------

handleListConvos
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleListConvos req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just _  -> return $ XrpcSuccess "{\"convos\":[]}"
