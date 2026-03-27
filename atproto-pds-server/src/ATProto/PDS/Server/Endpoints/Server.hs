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
import           Data.IORef                 (readIORef, modifyIORef')
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.Lex.Codec               (Codec)
import qualified ATProto.Lex.Codec    as Codec
import qualified ATProto.Lex.Json     as LexJson

import ATProto.Crypto.Types          (PrivKey)
import ATProto.OAuth.Provider.Token  (SigningKey, CreateTokenParams (..),
                                      createAccessToken)
import ATProto.PDS.Storage           (BlockStore, RepoStore)
import ATProto.PDS.Repo              (initRepo)
import ATProto.Syntax.DID            (DID, unDID, parseDID)
import ATProto.Syntax.TID            (tidNow, unTID)
import ATProto.XRPC.Server.Types     (XrpcServerRequest (..), XrpcHandlerResult (..))
import ATProto.PDS.Server.Env

-- ---------------------------------------------------------------------------
-- Types and codecs
-- ---------------------------------------------------------------------------

data DescribeServerResponse = DescribeServerResponse
  { dsrAvailableUserDomains :: [T.Text]
  , dsrInviteCodeRequired   :: Bool
  }

describeServerResponseCodec :: Codec DescribeServerResponse
describeServerResponseCodec =
    Codec.record "com.atproto.server.describeServer#response" $
        DescribeServerResponse
            <$> Codec.requiredField "availableUserDomains" (Codec.array Codec.text) dsrAvailableUserDomains
            <*> Codec.requiredField "inviteCodeRequired"   Codec.bool              dsrInviteCodeRequired

data CreateSessionRequest = CreateSessionRequest
  { csrqIdentifier :: T.Text
  , csrqPassword   :: T.Text
  }

createSessionRequestCodec :: Codec CreateSessionRequest
createSessionRequestCodec =
    Codec.record "com.atproto.server.createSession" $
        CreateSessionRequest
            <$> Codec.requiredField "identifier" Codec.text csrqIdentifier
            <*> Codec.requiredField "password"   Codec.text csrqPassword

data CreateSessionResponse = CreateSessionResponse
  { csrDid        :: T.Text
  , csrHandle     :: T.Text
  , csrAccessJwt  :: T.Text
  , csrRefreshJwt :: T.Text
  }

createSessionResponseCodec :: Codec CreateSessionResponse
createSessionResponseCodec =
    Codec.record "com.atproto.server.createSession#response" $
        CreateSessionResponse
            <$> Codec.requiredField "did"        Codec.text csrDid
            <*> Codec.requiredField "handle"     Codec.text csrHandle
            <*> Codec.requiredField "accessJwt"  Codec.text csrAccessJwt
            <*> Codec.requiredField "refreshJwt" Codec.text csrRefreshJwt

data GetSessionResponse = GetSessionResponse
  { gsrDid    :: T.Text
  , gsrHandle :: T.Text
  }

getSessionResponseCodec :: Codec GetSessionResponse
getSessionResponseCodec =
    Codec.record "com.atproto.server.getSession#response" $
        GetSessionResponse
            <$> Codec.requiredField "did"    Codec.text gsrDid
            <*> Codec.requiredField "handle" Codec.text gsrHandle

-- ---------------------------------------------------------------------------
-- com.atproto.server.describeServer
-- ---------------------------------------------------------------------------

handleDescribeServer
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleDescribeServer _req = do
  hostname <- asks envHostname
  let resp = DescribeServerResponse
        { dsrAvailableUserDomains = [hostname]
        , dsrInviteCodeRequired   = False
        }
  return $ XrpcSuccess (LexJson.encode describeServerResponseCodec resp)

-- ---------------------------------------------------------------------------
-- com.atproto.server.createSession
-- ---------------------------------------------------------------------------

handleCreateSession
  :: (BlockStore s, RepoStore s)
  => XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleCreateSession req = do
  case xsrBody req of
    Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "Request body required")
    Just body ->
      case LexJson.decode createSessionRequestCodec body of
        Left err -> return $ XrpcHandlerError "InvalidRequest" (Just (T.pack err))
        Right parsed -> doCreateSession (csrqIdentifier parsed) (csrqPassword parsed)

doCreateSession
  :: (BlockStore s, RepoStore s)
  => T.Text -> T.Text -> AppM s XrpcHandlerResult
doCreateSession ident password = do
  env <- asks id
  mDid <- liftIO $ lookupDIDForIdentifier env ident
  case mDid of
    Nothing -> do
      -- Auto-create account if identifier looks like a handle.
      if "did:" `T.isPrefixOf` ident
        then return $ XrpcHandlerError "InvalidRequest" (Just "Account not found")
        else autoCreateAccount ident password
    Just did -> do
      accts <- liftIO $ readIORef (envAccounts env)
      case Map.lookup (unDID did) accts of
        Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "Account not found")
        Just ai
          | aiPassword ai /= password ->
              return $ XrpcHandlerError "AuthenticationRequired" (Just "Invalid password")
          | not (aiActive ai) ->
              return $ XrpcHandlerError "AccountDeactivated" (Just "Account is deactivated")
          | otherwise ->
              issueSession did (aiHandle ai)

autoCreateAccount
  :: (BlockStore s, RepoStore s)
  => T.Text -> T.Text -> AppM s XrpcHandlerResult
autoCreateAccount handle password = do
  env <- asks id
  tid <- liftIO $ unTID <$> tidNow
  let didText = "did:plc:" <> tid
  case parseDID didText of
    Left _ -> return $ XrpcHandlerError "InternalError" (Just "Failed to generate DID")
    Right did -> do
      -- Init the repo.
      res <- liftIO $ initRepo (envStore env) did (envSigningKey env)
      case res of
        Left err -> return $ XrpcHandlerError "InternalError"
          (Just (T.pack (show err)))
        Right _ -> do
          let ai = AccountInfo did handle password True
          liftIO $ modifyIORef' (envAccounts env)
            (Map.insert (unDID did) ai)
          liftIO $ modifyIORef' (envHandleIndex env)
            (Map.insert handle did)
          issueSession did handle

issueSession :: DID -> T.Text -> AppM s XrpcHandlerResult
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
    Left err -> return $ XrpcHandlerError "InternalError" (Just (T.pack err))
    Right accessJwt -> do
      refreshTid <- liftIO $ unTID <$> tidNow
      let refreshJwt = "refresh-" <> refreshTid
          si = SessionInfo (unDID did) handle accessJwt refreshJwt
      liftIO $ modifyIORef' (envSessions env)
        (Map.insert accessJwt si)
      liftIO $ modifyIORef' (envRefreshTokens env)
        (Map.insert refreshJwt si)
      let resp = CreateSessionResponse
            { csrDid        = unDID did
            , csrHandle     = handle
            , csrAccessJwt  = accessJwt
            , csrRefreshJwt = refreshJwt
            }
      return $ XrpcSuccess (LexJson.encode createSessionResponseCodec resp)

-- ---------------------------------------------------------------------------
-- com.atproto.server.getSession
-- ---------------------------------------------------------------------------

handleGetSession
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleGetSession req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just did -> do
      env <- asks id
      mHandle <- liftIO $ lookupHandle env =<< maybeParseDID did
      let handle = maybe did id mHandle
          resp = GetSessionResponse
            { gsrDid    = did
            , gsrHandle = handle
            }
      return $ XrpcSuccess (LexJson.encode getSessionResponseCodec resp)
  where
    maybeParseDID t = case parseDID t of
      Right d -> return d
      Left _  -> fail "bad DID"

-- ---------------------------------------------------------------------------
-- com.atproto.server.refreshSession
-- ---------------------------------------------------------------------------

handleRefreshSession
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleRefreshSession req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just callerDid -> do
      env <- asks id
      -- Re-issue tokens for the caller.
      case parseDID callerDid of
        Left _ -> return $ XrpcHandlerError "InvalidRequest" (Just "Invalid DID")
        Right did -> do
          mHandle <- liftIO $ lookupHandle env did
          case mHandle of
            Nothing -> return $ XrpcHandlerError "InvalidRequest" (Just "Account not found")
            Just handle -> issueSession did handle

-- ---------------------------------------------------------------------------
-- com.atproto.server.activateAccount
-- ---------------------------------------------------------------------------

handleActivateAccount
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleActivateAccount req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just callerDid -> do
      env <- asks id
      liftIO $ modifyIORef' (envAccounts env)
        (Map.adjust (\ai -> ai { aiActive = True }) callerDid)
      return XrpcAccepted

-- ---------------------------------------------------------------------------
-- com.atproto.server.deactivateAccount
-- ---------------------------------------------------------------------------

handleDeactivateAccount
  :: XrpcServerRequest T.Text -> AppM s XrpcHandlerResult
handleDeactivateAccount req =
  case xsrCaller req of
    Nothing -> return $ XrpcHandlerError "AuthRequired" (Just "Authentication required")
    Just callerDid -> do
      env <- asks id
      liftIO $ modifyIORef' (envAccounts env)
        (Map.adjust (\ai -> ai { aiActive = False }) callerDid)
      return XrpcAccepted
