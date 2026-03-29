-- | @com.atproto.server.*@ endpoint handlers.
module ATProto.PDS.Server.Endpoints.Server
  ( handleDescribeServer
  , handleCreateSession
  , handleGetSession
  , handleRefreshSession
  , handleActivateAccount
  , handleDeactivateAccount
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import           Data.IORef                 (modifyIORef')
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.OAuth.Provider.Token  (CreateTokenParams (..),
                                      createAccessToken)
import ATProto.PDS.Storage           (BlockStore, RepoStore,
                                      AccountStore (..), AccountInfo (..))
import ATProto.PDS.Repo              (initRepo)
import ATProto.Repo.Server           (DescribeServerResponse (..),
                                      describeServerResponseCodec,
                                      CreateSessionRequest (..),
                                      createSessionRequestCodec,
                                      CreateSessionResponse (..),
                                      createSessionResponseCodec,
                                      GetSessionResponse (..),
                                      getSessionResponseCodec)
import ATProto.Syntax.DID            (DID, unDID, parseDID)
import ATProto.Syntax.TID            (tidNow, unTID)
import ATProto.XRPC.Server           (XrpcServerRequest (..), XrpcHandlerResult (..),
                                      runHandler, requireAuth, decodeBody,
                                      respondCodec, throwXrpc, Handler)
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- com.atproto.server.describeServer
-- ---------------------------------------------------------------------------

handleDescribeServer
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleDescribeServer _req = runHandler $ do
  hostname <- asks envHostname
  respondCodec describeServerResponseCodec $
    DescribeServerResponse [hostname] False

-- ---------------------------------------------------------------------------
-- com.atproto.server.createSession
-- ---------------------------------------------------------------------------

handleCreateSession
  :: (BlockStore s, RepoStore s, AccountStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleCreateSession req = runHandler $ do
  parsed <- decodeBody createSessionRequestCodec req
  doCreateSession (csrqIdentifier parsed) (csrqPassword parsed)

doCreateSession
  :: (BlockStore s, RepoStore s, AccountStore s)
  => T.Text -> T.Text -> Handler (AppM s) XrpcHandlerResult
doCreateSession ident password = do
  store <- asks envStore
  -- Try handle index first, then DID lookup.
  mDid <- liftIO $ do
    mByHandle <- lookupByHandle store ident
    case mByHandle of
      Just did -> return (Just did)
      Nothing  -> do
        mAcct <- getAccount store ident
        return (aiDid <$> mAcct)
  case mDid of
    Nothing ->
      -- Auto-create account if identifier looks like a handle.
      if "did:" `T.isPrefixOf` ident
        then throwXrpc "InvalidRequest" "Account not found"
        else autoCreateAccount ident password
    Just did -> do
      store' <- asks envStore
      mAcct <- liftIO $ getAccount store' (unDID did)
      case mAcct of
        Nothing -> throwXrpc "InvalidRequest" "Account not found"
        Just ai
          | aiPassword ai /= password ->
              throwXrpc "AuthenticationRequired" "Invalid password"
          | not (aiActive ai) ->
              throwXrpc "AccountDeactivated" "Account is deactivated"
          | otherwise ->
              issueSession did (aiHandle ai)

autoCreateAccount
  :: (BlockStore s, RepoStore s, AccountStore s)
  => T.Text -> T.Text -> Handler (AppM s) XrpcHandlerResult
autoCreateAccount handle password = do
  env <- asks id
  tid <- liftIO $ unTID <$> tidNow
  let didText = "did:plc:" <> tid
  did <- case parseDID didText of
    Left _ -> throwXrpc "InternalError" "Failed to generate DID"
    Right d -> return d
  -- Init the repo.
  res <- liftIO $ initRepo (envStore env) did (envSigningKey env)
  case res of
    Left err -> throwXrpc "InternalError" (T.pack (show err))
    Right _ -> do
      let ai = AccountInfo did handle password True
      liftIO $ putAccount (envStore env) (unDID did) ai
      liftIO $ registerHandle (envStore env) handle did
      issueSession did handle

issueSession :: DID -> T.Text -> Handler (AppM s) XrpcHandlerResult
issueSession did handle = do
  env <- asks id
  let tokenParams = CreateTokenParams
        { ctpSub      = unDID did
        , ctpAud      = envIssuer env
        , ctpScope    = "atproto transition:generic"
        , ctpClientId = Nothing
        , ctpCnfJkt   = Nothing
        , ctpLifetime = 7200
        }
  eAccess <- liftIO $ createAccessToken (envTokenKey env) (envIssuer env) tokenParams
  case eAccess of
    Left err -> throwXrpc "InternalError" (T.pack err)
    Right accessJwt -> do
      refreshTid <- liftIO $ unTID <$> tidNow
      let refreshJwt = "refresh-" <> refreshTid
          si = SessionInfo (unDID did) handle accessJwt refreshJwt
      liftIO $ modifyIORef' (envSessions env) (Map.insert accessJwt si)
      liftIO $ modifyIORef' (envRefreshTokens env) (Map.insert refreshJwt si)
      respondCodec createSessionResponseCodec $
        CreateSessionResponse (unDID did) handle accessJwt refreshJwt

-- ---------------------------------------------------------------------------
-- com.atproto.server.getSession
-- ---------------------------------------------------------------------------

handleGetSession
  :: AccountStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetSession req = runHandler $ do
  callerDid <- requireAuth req
  store <- asks envStore
  mAcct <- liftIO $ getAccount store callerDid
  let handle = maybe callerDid aiHandle mAcct
  respondCodec getSessionResponseCodec $
    GetSessionResponse callerDid handle

-- ---------------------------------------------------------------------------
-- com.atproto.server.refreshSession
-- ---------------------------------------------------------------------------

handleRefreshSession
  :: AccountStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleRefreshSession req = runHandler $ do
  callerDid <- requireAuth req
  did <- case parseDID callerDid of
    Left _  -> throwXrpc "InvalidRequest" "Invalid DID"
    Right d -> return d
  store <- asks envStore
  mAcct <- liftIO $ getAccount store callerDid
  case mAcct of
    Nothing -> throwXrpc "InvalidRequest" "Account not found"
    Just ai -> issueSession did (aiHandle ai)

-- ---------------------------------------------------------------------------
-- com.atproto.server.activateAccount
-- ---------------------------------------------------------------------------

handleActivateAccount
  :: AccountStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleActivateAccount req = runHandler $ do
  callerDid <- requireAuth req
  store <- asks envStore
  liftIO $ modifyAccount store callerDid (\ai -> ai { aiActive = True })
  return XrpcAccepted

-- ---------------------------------------------------------------------------
-- com.atproto.server.deactivateAccount
-- ---------------------------------------------------------------------------

handleDeactivateAccount
  :: AccountStore s
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleDeactivateAccount req = runHandler $ do
  callerDid <- requireAuth req
  store <- asks envStore
  liftIO $ modifyAccount store callerDid (\ai -> ai { aiActive = False })
  return XrpcAccepted
