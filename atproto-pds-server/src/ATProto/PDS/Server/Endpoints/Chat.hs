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

import ATProto.Repo.Chat            (GetLogResponse (..),
                                     getLogResponseCodec,
                                     ListConvosResponse (..),
                                     listConvosResponseCodec)
import ATProto.XRPC.Server          (XrpcServerRequest (..), XrpcHandlerResult (..),
                                     runHandler, requireAuth, respondCodec)
import ATProto.PDS.Server.Env       (AppM)

-- ---------------------------------------------------------------------------
-- chat.bsky.convo.getLog
-- ---------------------------------------------------------------------------

handleGetLog
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetLog req = runHandler $ do
  _ <- requireAuth req
  respondCodec getLogResponseCodec $ GetLogResponse []

-- ---------------------------------------------------------------------------
-- chat.bsky.convo.listConvos
-- ---------------------------------------------------------------------------

handleListConvos
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleListConvos req = runHandler $ do
  _ <- requireAuth req
  respondCodec listConvosResponseCodec $ ListConvosResponse []
