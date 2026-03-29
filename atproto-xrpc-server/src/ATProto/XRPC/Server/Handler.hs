-- | Handler combinators for XRPC server endpoints.
--
-- Provides an 'EitherT'-based 'Handler' monad that eliminates nested
-- @case@ / @Either@ chains in endpoint implementations.
--
-- = Usage
--
-- @
-- handleGetRecord :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
-- handleGetRecord req = runHandler $ do
--   repo       <- requireParam "repo" req
--   collection <- requireParam "collection" req
--   rkey       <- requireParam "rkey" req
--   did        <- parseDIDParam repo
--   headCid    <- liftAction $ getRepoHead store did
--   ...
--   respondCodec getRecordResponseCodec resp
-- @
module ATProto.XRPC.Server.Handler
  ( -- * Handler monad
    Handler
  , runHandler
    -- * Auth helpers
  , requireAuth
    -- * Request body helpers
  , requireBody
  , decodeBody
    -- * Parameter helpers
  , requireParam
    -- * Error helpers
  , throwXrpc
    -- * Response helpers
  , respondCodec
  , respondAccepted
  , respondRaw
    -- * IO helpers
  , liftAction
  ) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Either (EitherT, runEitherT, left)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T

import ATProto.Lex.Codec   (Codec)
import qualified ATProto.Lex.Json as LexJson

import ATProto.XRPC.Server.Types (XrpcServerRequest (..), XrpcHandlerResult (..))

-- | The handler monad: 'EitherT' over the application monad, short-circuiting
-- on 'XrpcHandlerResult' (which carries error responses).
type Handler m = EitherT XrpcHandlerResult m

-- | Run a 'Handler' computation, returning the 'XrpcHandlerResult'.
--
-- On 'Left', the error result is returned directly.
-- On 'Right', the success result is returned.
runHandler :: Monad m => Handler m XrpcHandlerResult -> m XrpcHandlerResult
runHandler h = do
  result <- runEitherT h
  case result of
    Left  err -> return err
    Right ok  -> return ok

-- | Require an authenticated caller.  Short-circuits with 401 if absent.
requireAuth :: Monad m => XrpcServerRequest did -> Handler m did
requireAuth req =
  case xsrCaller req of
    Nothing  -> left $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just did -> return did

-- | Require a request body.  Short-circuits with 400 if absent.
requireBody :: Monad m => XrpcServerRequest did -> Handler m BL.ByteString
requireBody req =
  case xsrBody req of
    Nothing   -> left $ XrpcHandlerError "InvalidRequest" (Just "Request body required")
    Just body -> return body

-- | Decode a request body using a 'Codec'.  Short-circuits with 400 on failure.
decodeBody :: Monad m => Codec a -> XrpcServerRequest did -> Handler m a
decodeBody codec req = do
  body <- requireBody req
  case LexJson.decode codec body of
    Left err -> left $ XrpcHandlerError "InvalidRequest" (Just (T.pack err))
    Right a  -> return a

-- | Require a query parameter.  Short-circuits with 400 if absent.
requireParam :: Monad m => T.Text -> XrpcServerRequest did -> Handler m T.Text
requireParam name req =
  case Map.lookup name (xsrParams req) of
    Nothing -> left $ XrpcHandlerError "InvalidRequest"
                 (Just (name <> " parameter required"))
    Just v  -> return v

-- | Throw an XRPC error, short-circuiting the handler.
throwXrpc :: Monad m => T.Text -> T.Text -> Handler m a
throwXrpc code msg = left $ XrpcHandlerError code (Just msg)

-- | Encode a response using a 'Codec' and return HTTP 200.
respondCodec :: Monad m => Codec a -> a -> Handler m XrpcHandlerResult
respondCodec codec = return . XrpcSuccess . LexJson.encode codec

-- | Return HTTP 202 Accepted.
respondAccepted :: Monad m => Handler m XrpcHandlerResult
respondAccepted = return XrpcAccepted

-- | Return a raw 'BL.ByteString' body with HTTP 200.
respondRaw :: Monad m => BL.ByteString -> Handler m XrpcHandlerResult
respondRaw = return . XrpcSuccess

-- | Lift an IO action that returns @Either error a@ into the handler.
-- The error is converted to an XRPC error string.
liftAction :: MonadIO m => IO (Either e a) -> (e -> (T.Text, T.Text)) -> Handler m a
liftAction action toErr = do
  result <- liftIO action
  case result of
    Left err -> let (code, msg) = toErr err in throwXrpc code msg
    Right a  -> return a
