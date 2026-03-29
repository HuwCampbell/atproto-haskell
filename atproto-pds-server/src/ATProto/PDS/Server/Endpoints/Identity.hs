-- | @com.atproto.identity.*@ endpoint handlers.
module ATProto.PDS.Server.Endpoints.Identity
  ( handleResolveHandle
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.Text            as T

import ATProto.PDS.Storage           (AccountStore (..))
import ATProto.Syntax.DID            (unDID)
import ATProto.Repo.Identity         (ResolveHandleResponse (..),
                                      resolveHandleResponseCodec)
import ATProto.XRPC.Server           (XrpcServerRequest (..), XrpcHandlerResult (..), lift,
                                      runHandler, requireParam, respondCodec, throwXrpc)
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- com.atproto.identity.resolveHandle
-- ---------------------------------------------------------------------------

handleResolveHandle
  :: AccountStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleResolveHandle req = runHandler $ do
  handle <- requireParam "handle" req
  store <- lift $ asks envStore
  mDid <- liftIO $ lookupByHandle store handle
  case mDid of
    Nothing -> throwXrpc "HandleNotFound" ("Unable to resolve handle: " <> handle)
    Just did -> respondCodec resolveHandleResponseCodec $
      ResolveHandleResponse (unDID did)
