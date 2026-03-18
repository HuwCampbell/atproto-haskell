-- | ATProto OAuth 2.1 client.
--
-- Implements the full ATProto OAuth flow with the following mandatory
-- extensions: PKCE (RFC 7636), Pushed Authorization Requests (RFC 9126),
-- and Demonstrated Proof of Possession (RFC 9449).
--
-- = Usage
--
-- @
-- import ATProto.OAuth
--
-- main :: IO ()
-- main = do
--   ss  <- newInMemoryStateStore
--   ses <- newInMemorySessionStore
--   let cfg = OAuthClientConfig
--         { occMetadata         = defaultClientMetadata "https://myapp.example.com"
--                                   ["https://myapp.example.com/callback"]
--         , occManager          = ...  -- newManager tlsManagerSettings
--         , occResolveHandle    = resolveHandle resolver
--         , occResolveDid       = \did -> ...
--         , occTokenRefreshBuffer = 60
--         }
--   client <- newOAuthClient cfg ss ses
--   result <- authorize client AuthorizeParams
--     { apInput       = "alice.bsky.social"
--     , apRedirectUri = "https://myapp.example.com/callback"
--     , apScope       = "transition:generic"
--     , apAppState    = Nothing
--     }
--   ...
-- @
module ATProto.OAuth.Client
  ( -- * Stores
    StateStore (..)
  , newInMemoryStateStore
  , SessionStore (..)
  , newInMemorySessionStore
    -- * Client configuration
  , OAuthClientConfig (..)
  , DidDocumentLike (..)
    -- * Client
  , OAuthClient
  , newOAuthClient
    -- * Accessor for the nonce cache
  , clientNonceCache
    -- * State and session data
  , StateData (..)
  , Session (..)
    -- * Authorization
  , AuthorizeParams (..)
  , AuthorizeResult (..)
  , authorize
    -- * Callback
  , CallbackParams (..)
  , callback
    -- * Session access
  , getTokenInfo
  , deleteSession
  ) where

import           Control.Monad                (unless)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Either   (newEitherT, runEitherT, hoistEither,
                                               hoistMaybe, left)
import           Control.Exception            (SomeException, catch)
import           Data.IORef                   (IORef, modifyIORef', newIORef,
                                               readIORef)
import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Types              as AesonTypes
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BC
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Char                    as Char
import qualified Data.CaseInsensitive         as CI
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Time                    (NominalDiffTime, UTCTime,
                                               addUTCTime, getCurrentTime)
import           Network.HTTP.Client          (Manager, RequestBody (..),
                                               httpLbs, method, parseRequest,
                                               requestBody, requestHeaders,
                                               responseBody, responseHeaders,
                                               responseStatus)
import           Network.HTTP.Types           (renderQuery, statusCode, toQuery)

import           ATProto.OAuth.DPoP
import           ATProto.OAuth.PKCE
import           ATProto.OAuth.ServerMetadata
import           ATProto.OAuth.Types

-- ---------------------------------------------------------------------------
-- Stores
-- ---------------------------------------------------------------------------

-- | A key-value store for pending authorization state.
data StateStore = StateStore
  { storeGetState :: T.Text -> IO (Maybe StateData)
  , storePutState :: T.Text -> StateData -> IO ()
  , storeDelState :: T.Text -> IO ()
  }

-- | A key-value store for active sessions.
data SessionStore = SessionStore
  { sessStoreGet :: T.Text -> IO (Maybe Session)
  , sessStorePut :: T.Text -> Session -> IO ()
  , sessStoreDel :: T.Text -> IO ()
  }

-- | Create an in-memory 'StateStore' backed by an 'IORef'.
newInMemoryStateStore :: IO StateStore
newInMemoryStateStore = do
  ref <- newIORef (Map.empty :: Map.Map T.Text StateData)
  return StateStore
    { storeGetState = \k   -> fmap (Map.lookup k) (readIORef ref)
    , storePutState = \k v -> modifyIORef' ref (Map.insert k v)
    , storeDelState = \k   -> modifyIORef' ref (Map.delete k)
    }

-- | Create an in-memory 'SessionStore' backed by an 'IORef'.
newInMemorySessionStore :: IO SessionStore
newInMemorySessionStore = do
  ref <- newIORef (Map.empty :: Map.Map T.Text Session)
  return SessionStore
    { sessStoreGet = \k   -> fmap (Map.lookup k) (readIORef ref)
    , sessStorePut = \k v -> modifyIORef' ref (Map.insert k v)
    , sessStoreDel = \k   -> modifyIORef' ref (Map.delete k)
    }

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | A minimal view of a DID document: just the PDS endpoint.
--
-- The caller extracts this from the full DID document before passing it to
-- the client.  This avoids a hard dependency on @atproto-haskell-did@.
data DidDocumentLike = DidDocumentLike
  { ddlPdsUrl :: T.Text
    -- ^ URL of the user's Personal Data Server service endpoint.
  } deriving (Eq, Show)

-- | Configuration for the 'OAuthClient'.
data OAuthClientConfig = OAuthClientConfig
  { occMetadata           :: OAuthClientMetadata
    -- ^ The client's own metadata (served at the @client_id@ URL).
  , occManager            :: Manager
    -- ^ Shared TLS-capable HTTP connection manager.
  , occResolveHandle      :: T.Text -> IO (Maybe T.Text)
    -- ^ Resolve a handle to a DID.  Pass @const (return Nothing)@ if only
    -- DIDs will be passed to 'authorize'.
  , occResolveDid         :: T.Text -> IO (Either String DidDocumentLike)
    -- ^ Resolve a DID to a minimal DID document.  Must accept both @did:plc@
    -- and @did:web@ identifiers.
  , occTokenRefreshBuffer :: NominalDiffTime
    -- ^ Refresh tokens this many seconds before they expire.  Default: 60.
  }

-- ---------------------------------------------------------------------------
-- Client
-- ---------------------------------------------------------------------------

-- | An OAuth client.  Create with 'newOAuthClient'.
data OAuthClient = OAuthClient
  { ocConfig       :: OAuthClientConfig
  , ocStateStore   :: StateStore
  , ocSessionStore :: SessionStore
  , ocNonceCache   :: IORef (Map.Map T.Text T.Text)
    -- ^ Per-issuer DPoP nonce cache: issuer URL → server-sent nonce.
  }

-- | Create a new 'OAuthClient'.
newOAuthClient
  :: OAuthClientConfig
  -> StateStore
  -> SessionStore
  -> IO OAuthClient
newOAuthClient cfg ss ses = do
  nonceRef <- newIORef Map.empty
  return OAuthClient
    { ocConfig       = cfg
    , ocStateStore   = ss
    , ocSessionStore = ses
    , ocNonceCache   = nonceRef
    }

clientNonceCache :: OAuthClient -> IORef (Map.Map T.Text T.Text)
clientNonceCache = ocNonceCache

-- ---------------------------------------------------------------------------
-- State and session data
-- ---------------------------------------------------------------------------

-- | Data held between the authorization redirect and the callback.
data StateData = StateData
  { sdIss          :: T.Text
    -- ^ Expected issuer URL.
  , sdDpopKey      :: DpopKey
    -- ^ Ephemeral DPoP keypair for this authorization flow.
  , sdCodeVerifier :: BS.ByteString
    -- ^ PKCE code verifier (sent in the token exchange).
  , sdState        :: T.Text
    -- ^ @state@ value (prevents CSRF).
  , sdAppState     :: Maybe T.Text
    -- ^ Optional application-level state to restore after the redirect.
  }

-- | An active OAuth session.
data Session = Session
  { sessTokenSet :: TokenSet
    -- ^ Current token set.
  , sessDpopKey  :: DpopKey
    -- ^ DPoP key bound to this session.
  }

-- ---------------------------------------------------------------------------
-- Authorization
-- ---------------------------------------------------------------------------

-- | Parameters for starting an authorization flow.
data AuthorizeParams = AuthorizeParams
  { apInput       :: T.Text
    -- ^ Handle or DID of the user.
  , apRedirectUri :: T.Text
    -- ^ Redirect URI for the callback.
  , apScope       :: T.Text
    -- ^ Requested scope; use @\"transition:generic\"@ for broad access.
  , apAppState    :: Maybe T.Text
    -- ^ Optional opaque application state.
  }

-- | Result of a successful 'authorize' call.
data AuthorizeResult = AuthorizeResult
  { arRedirectUrl :: T.Text
    -- ^ Redirect the user's browser to this URL.
  , arState       :: T.Text
    -- ^ @state@ parameter; correlates the callback with this flow.
  } deriving (Eq, Show)

-- | Begin an authorization flow.
--
-- Resolves the user's identity, discovers the authorization server,
-- generates PKCE and DPoP credentials, issues a PAR request (RFC 9126),
-- and returns the redirect URL.
authorize
  :: OAuthClient
  -> AuthorizeParams
  -> IO (Either OAuthError AuthorizeResult)
authorize client params = runEitherT $ do
  did          <- newEitherT  (resolveToDid client (apInput params))
  let mgr       = occManager  (ocConfig client)
      cm        = occMetadata (ocConfig client)
  pdsUrl       <- newEitherT  (resolvePds client did)
  issuer       <- newEitherT  (getIssuerForPds mgr pdsUrl)
  asMeta       <- newEitherT  (fetchAuthorizationServerMetadata mgr issuer)
  verifier     <- lift $ generateCodeVerifier
  let challenge = codeChallenge verifier
  dpopKey      <- lift $ generateDpopKey
  stateVal     <- lift $ generateJti
  reqUri       <- newEitherT $ doPar client asMeta dpopKey challenge stateVal params
  let sd = StateData
        { sdIss          = issuer
        , sdDpopKey      = dpopKey
        , sdCodeVerifier = verifier
        , sdState        = stateVal
        , sdAppState     = apAppState params
        }
  lift $ storePutState (ocStateStore client) stateVal sd
  let authUrl = buildAuthRedirectUrl asMeta cm stateVal reqUri
  return $ AuthorizeResult
    { arRedirectUrl = authUrl
    , arState       = stateVal
    }

-- | POST a PAR request and return the @request_uri@.
doPar
  :: OAuthClient
  -> OAuthAuthorizationServerMetadata
  -> DpopKey
  -> BS.ByteString   -- code_challenge
  -> T.Text          -- state
  -> AuthorizeParams
  -> IO (Either OAuthError T.Text)
doPar client asMeta dpopKey challenge stateVal params =
  case asmPushedAuthorizationRequestEndpoint asMeta of
    Nothing ->
      return (Left (OAuthServerError "par_not_supported"
                      (Just "Server does not support PAR") 0))
    Just endpoint -> do
      let mgr = occManager (ocConfig client)
          cm  = occMetadata (ocConfig client)
          iss = asmIssuer asMeta
      cachedNonce <- fmap (Map.lookup iss) (readIORef (ocNonceCache client))
      eProof <- createDpopProof dpopKey DpopClaims
        { dcHtm   = "POST"
        , dcHtu   = endpoint
        , dcNonce = cachedNonce
        , dcAth   = Nothing
        }
      case eProof of
        Left err -> return (Left (OAuthDpopError err))
        Right proof -> do
          let formBody =
                [ ("response_type",         "code")
                , ("client_id",             TE.encodeUtf8 (cmClientId cm))
                , ("redirect_uri",          TE.encodeUtf8 (apRedirectUri params))
                , ("scope",                 TE.encodeUtf8 (apScope params))
                , ("code_challenge",        challenge)
                , ("code_challenge_method", "S256")
                , ("state",                 TE.encodeUtf8 stateVal)
                , ("login_hint",            TE.encodeUtf8 (apInput params))
                ]
              dpopHdr = [("DPoP", TE.encodeUtf8 proof)]
          (respHdrs, eResp) <- doPost mgr (T.unpack endpoint) formBody dpopHdr
          storeNonce client iss respHdrs
          case eResp of
            Left (OAuthServerError "use_dpop_nonce" _ _) -> do
              -- Retry once with the nonce the server sent in the error response.
              newNonce <- fmap (Map.lookup iss) (readIORef (ocNonceCache client))
              eProof2 <- createDpopProof dpopKey DpopClaims
                { dcHtm   = "POST"
                , dcHtu   = endpoint
                , dcNonce = newNonce
                , dcAth   = Nothing
                }

              case eProof2 of
                Left err2 -> return (Left (OAuthDpopError err2))
                Right proof2 -> do

                  let dpopHdr2 = [("DPoP", TE.encodeUtf8 proof2)]
                  (respHdrs2, eResp2) <- doPost mgr (T.unpack endpoint) formBody dpopHdr2
                  storeNonce client iss respHdrs2
                  case eResp2 of
                    Left err2  -> return (Left err2)
                    Right body -> do
                      parseParResponse body
            Left err -> return (Left err)
            Right body -> parseParResponse body

-- | Parse the @request_uri@ from a PAR response body.
parseParResponse :: BL.ByteString -> IO (Either OAuthError T.Text)
parseParResponse body =
  case Aeson.eitherDecode body of
    Left msg -> return (Left (OAuthParseError msg))
    Right (Aeson.Object o) ->
      case AesonTypes.parseMaybe (Aeson..: "request_uri") o of
        Just (Aeson.String uri) -> return (Right uri)
        _ -> return (Left (OAuthParseError "Missing request_uri in PAR response"))
    Right _ -> return (Left (OAuthParseError "PAR response is not a JSON object"))

-- | Build the authorization redirect URL using the @request_uri@ from PAR.
buildAuthRedirectUrl
  :: OAuthAuthorizationServerMetadata
  -> OAuthClientMetadata
  -> T.Text  -- state (unused; already bound in request_uri)
  -> T.Text  -- request_uri
  -> T.Text
buildAuthRedirectUrl asMeta cm _stateVal reqUri =
  let base  = asmAuthorizationEndpoint asMeta
      query = TE.decodeUtf8Lenient $ renderQuery True $ toQuery
                [ ("client_id"   :: BS.ByteString, TE.encodeUtf8 (cmClientId cm))
                , ("request_uri" :: BS.ByteString, TE.encodeUtf8 reqUri)
                ]
  in  base <> query

-- ---------------------------------------------------------------------------
-- Callback
-- ---------------------------------------------------------------------------

-- | Parameters received from the authorization server callback redirect.
data CallbackParams = CallbackParams
  { cpCode  :: T.Text
    -- ^ Authorization code.
  , cpState :: T.Text
    -- ^ @state@ value (must match what was sent in 'authorize').
  , cpIss   :: Maybe T.Text
    -- ^ @iss@ parameter (RFC 9207; validate if present).
  } deriving (Eq, Show)

-- | Handle the authorization callback.
--
-- Exchanges the authorization code for tokens and performs the mandatory
-- issuer verification step.
callback :: OAuthClient -> CallbackParams -> IO (Either OAuthError Session)
callback client cp = do
  mSd <- storeGetState (ocStateStore client) (cpState cp)
  case mSd of
    Nothing -> return (Left OAuthStateNotFound)
    Just sd -> do
      let issOk = case cpIss cp of
                    Nothing   -> True
                    Just iss' -> iss' == sdIss sd
      if not issOk then
        return (Left OAuthStateMismatch)
      else do
        storeDelState (ocStateStore client) (cpState cp)
        eSession <- exchangeCode client sd (cpCode cp)
        case eSession of
          Left err      -> return (Left err)
          Right session -> do
            sessStorePut (ocSessionStore client)
                          (tsSub (sessTokenSet session)) session
            return (Right session)

-- | Exchange an authorization code for a 'Session'.
exchangeCode
  :: OAuthClient
  -> StateData
  -> T.Text     -- authorization code
  -> IO (Either OAuthError Session)
exchangeCode client sd code = runEitherT $ do
  let mgr = occManager (ocConfig client)
      cm  = occMetadata (ocConfig client)
      iss = sdIss sd
  asMeta <- newEitherT $ fetchAuthorizationServerMetadata mgr iss
  let tokenUrl    = asmTokenEndpoint asMeta
      redirectUri = case cmRedirectUris cm of
                      []    -> ""
                      (r:_) -> r
      formBody =
        [ ("grant_type",    "authorization_code")
        , ("code",          TE.encodeUtf8 code)
        , ("client_id",     TE.encodeUtf8 (cmClientId cm))
        , ("redirect_uri",  TE.encodeUtf8 redirectUri)
        , ("code_verifier", sdCodeVerifier sd)
        ]

  (body, _hdrs) <-
    newEitherT $
      postWithDpop client iss (sdDpopKey sd) tokenUrl formBody Nothing

  now <- lift $ getCurrentTime
  ts  <- hoistEither $ parseTokenResponse now iss body
  -- Issuer verification (security-critical).
  aud <- newEitherT $ verifyIssuer client ts
  let ts' = ts { tsAud = aud }
  return (Session
      { sessTokenSet = ts'
      , sessDpopKey  = sdDpopKey sd
      })

-- | Resolve the @sub@ DID and verify the issuer matches.
--
-- This is the critical security check: the user's PDS must be served by the
-- same authorization server that issued the token.
verifyIssuer :: OAuthClient -> TokenSet -> IO (Either OAuthError T.Text)
verifyIssuer client ts = runEitherT $ do
  let mgr = occManager (ocConfig client)
      sub = tsSub ts
      iss = tsIss ts
  pdsUrl         <- newEitherT $ resolvePds client sub
  resolvedIssuer <- newEitherT $ getIssuerForPds mgr pdsUrl
  unless (iss == resolvedIssuer) $
    left (OAuthIssuerMismatch iss resolvedIssuer)

  return pdsUrl

-- ---------------------------------------------------------------------------
-- Session access
-- ---------------------------------------------------------------------------

-- | Get the token set for a session, refreshing if near expiry.
--
-- Tokens are refreshed when the current time is within
-- 'occTokenRefreshBuffer' of the expiry time.
getTokenInfo
  :: OAuthClient
  -> T.Text      -- ^ DID of the user
  -> IO (Either OAuthError TokenSet)
getTokenInfo client did = do
  mSession <- sessStoreGet (ocSessionStore client) did
  case mSession of
    Nothing      -> return (Left OAuthStateNotFound)
    Just session -> do
      now <- getCurrentTime
      let ts     = sessTokenSet session
          buf    = occTokenRefreshBuffer (ocConfig client)
          stale  = case tsExpiresAt ts of
                     Nothing  -> False
                     Just expires -> addUTCTime (negate buf) expires <= now
      if stale
        then refreshSession client did session
        else return (Right ts)

-- | Refresh a session's tokens using the refresh token.
refreshSession
  :: OAuthClient
  -> T.Text   -- DID
  -> Session
  -> IO (Either OAuthError TokenSet)
refreshSession client did session = runEitherT $ do
  let mgr = occManager (ocConfig client)
      ts  = sessTokenSet session
      iss = tsIss ts
      cm  = occMetadata (ocConfig client)
  rt     <- hoistMaybe OAuthNoRefreshToken (tsRefreshToken ts)
  asMeta <- newEitherT (fetchAuthorizationServerMetadata mgr iss)

  let tokenUrl = asmTokenEndpoint asMeta
      formBody =
        [ ("grant_type",    "refresh_token")
        , ("refresh_token", TE.encodeUtf8 rt)
        , ("client_id",     TE.encodeUtf8 (cmClientId cm))
        ]

  (body, _hdrs) <-
    newEitherT (
      postWithDpop client iss (sessDpopKey session) tokenUrl formBody Nothing
    )

  now   <- lift getCurrentTime
  newTs <- hoistEither (parseTokenResponse now iss body)

  -- This is overly cautious as we verified the issuer on the initial login,
  -- but it's plausible the PDS has migrated to a new auth server.
  aud   <- newEitherT $ verifyIssuer client ts
  let newSession = session { sessTokenSet = newTs { tsAud = aud } }
  lift $ sessStorePut (ocSessionStore client) did newSession
  return newTs

-- | Remove a session from the session store.
deleteSession :: OAuthClient -> T.Text -> IO ()
deleteSession = sessStoreDel . ocSessionStore

-- ---------------------------------------------------------------------------
-- HTTP helpers
-- ---------------------------------------------------------------------------

-- | POST a form to @url@ with a DPoP proof, retrying once if the server
-- requests a nonce.
postWithDpop
  :: OAuthClient
  -> T.Text       -- issuer (for nonce cache)
  -> DpopKey
  -> T.Text       -- token endpoint URL
  -> [(BS.ByteString, BS.ByteString)]
  -> Maybe T.Text  -- access token (for ath; Nothing at token endpoint)
  -> IO (Either OAuthError (BL.ByteString, ResponseHeaders))
postWithDpop client issuer dpopKey tokenUrl formBody mAt = do
  cachedNonce <- fmap (Map.lookup issuer) (readIORef (ocNonceCache client))
  let ath = fmap (TE.decodeUtf8Lenient . accessTokenHash) mAt
  eProof <- buildProof dpopKey "POST" tokenUrl cachedNonce ath
  case eProof of
    Left err -> return (Left (OAuthDpopError err))
    Right proof -> do
      let extraHdrs = dpopRequestHeaders proof mAt
      (respHdrs, eResp) <- doPost (occManager (ocConfig client))
                                  (T.unpack tokenUrl) formBody extraHdrs
      storeNonce client issuer respHdrs
      case eResp of
        Right body ->
          return (Right (body, respHdrs))
        Left (OAuthServerError "use_dpop_nonce" _ _) -> do
          -- Retry once with the nonce the server sent in the error response.
          newNonce <- fmap (Map.lookup issuer) (readIORef (ocNonceCache client))
          eProof2 <- buildProof dpopKey "POST" tokenUrl newNonce ath
          case eProof2 of
            Left err2 -> return (Left (OAuthDpopError err2))
            Right proof2 -> do
              let extraHdrs2 = dpopRequestHeaders proof2 mAt
              (respHdrs2, eResp2) <- doPost (occManager (ocConfig client))
                                            (T.unpack tokenUrl) formBody extraHdrs2
              storeNonce client issuer respHdrs2
              case eResp2 of
                Right body2 ->
                  return (Right (body2, respHdrs2))
                Left err2 -> return (Left err2)
        Left err -> return (Left err)

-- | Build a DPoP proof for a POST request.
buildProof
  :: DpopKey
  -> T.Text       -- HTTP method
  -> T.Text       -- HTU (URL, no query/fragment)
  -> Maybe T.Text -- nonce
  -> Maybe T.Text -- ath
  -> IO (Either String T.Text)
buildProof dpopKey htm htu nonce ath =
  createDpopProof dpopKey DpopClaims
    { dcHtm   = htm
    , dcHtu   = T.takeWhile (/= '?') htu
    , dcNonce = nonce
    , dcAth   = ath
    }

-- | Build the request headers for a DPoP-protected endpoint.
dpopRequestHeaders :: T.Text -> Maybe T.Text -> [(BS.ByteString, BS.ByteString)]
dpopRequestHeaders proof Nothing   = [("DPoP", TE.encodeUtf8 proof)]
dpopRequestHeaders proof (Just at) =
  [ ("DPoP",          TE.encodeUtf8 proof)
  , ("Authorization", "DPoP " <> TE.encodeUtf8 at)
  ]

-- | Response headers type alias.
type ResponseHeaders = [(BS.ByteString, BS.ByteString)]

-- | Store the DPoP nonce from response headers into the per-issuer cache.
storeNonce :: OAuthClient -> T.Text -> ResponseHeaders -> IO ()
storeNonce client issuer hdrs =
  case lookup "dpop-nonce" (map normHeader hdrs) of
    Nothing    -> return ()
    Just nonce ->
      modifyIORef' (ocNonceCache client)
        (Map.insert issuer (TE.decodeUtf8Lenient nonce))
  where
    normHeader (k, v) = (BC.map Char.toLower k, v)

-- | POST a URL-encoded form and return the body + response headers.
--
-- Response headers are returned on /both/ success and failure, so that
-- the caller can always extract the @DPoP-Nonce@ header.
doPost
  :: Manager
  -> String
  -> [(BS.ByteString, BS.ByteString)]  -- form fields
  -> [(BS.ByteString, BS.ByteString)]  -- extra request headers
  -> IO (ResponseHeaders, Either OAuthError BL.ByteString)
doPost mgr url formFields extraHdrs =
  catch go (\e -> return ([], Left (OAuthNetworkError (show (e :: SomeException)))))
  where
    go :: IO (ResponseHeaders, Either OAuthError BL.ByteString)
    go = do
      req0 <- parseRequest url
      let encoded = renderQuery False (toQuery formFields)
          baseHdrs =
            [ (CI.mk "Content-Type", "application/x-www-form-urlencoded")
            , (CI.mk "Accept",       "application/json")
            ]
          xtraHdrs = map (\(k, v) -> (CI.mk k, v)) extraHdrs
          req = req0
            { method         = "POST"
            , requestBody    = RequestBodyBS encoded
            , requestHeaders = baseHdrs ++ xtraHdrs
            }
      resp <- httpLbs req mgr
      let status   = statusCode (responseStatus resp)
          body     = responseBody resp
          respHdrs = map (\(k, v) -> (CI.original k, v)) (responseHeaders resp)
      if status >= 200 && status < 300
        then return (respHdrs, Right body)
        else return (respHdrs, Left (parseError status body))

-- | Parse an OAuth error from a failed response.
parseError :: Int -> BL.ByteString -> OAuthError
parseError status body =
  case (Aeson.eitherDecode body :: Either String Aeson.Value) of
    Right (Aeson.Object o) ->
      let get k = case AesonTypes.parseMaybe (Aeson..: k) o of
                    Just (Aeson.String t) -> Just t
                    _                     -> Nothing
          errToken = maybe "Error" id (get "error")
          errMsg   = get "message"
      in  OAuthServerError errToken errMsg status
    _ -> OAuthServerError "Error" Nothing status

-- | Parse a token endpoint response into a 'TokenSet'.
parseTokenResponse
  :: UTCTime
  -> T.Text           -- issuer
  -> BL.ByteString
  -> Either OAuthError TokenSet
parseTokenResponse now iss body =
  case Aeson.eitherDecode body of
    Left msg -> Left (OAuthParseError msg)
    Right (Aeson.Object o) ->
      let getText k = case AesonTypes.parseMaybe (Aeson..: k) o of
                        Just (Aeson.String t) -> Just t
                        _                     -> Nothing
          getInt k  = case AesonTypes.parseMaybe (Aeson..: k) o of
                        Just (Aeson.Number n) ->
                          Just (round n :: Int)
                        _ -> Nothing
      in  case (getText "sub", getText "access_token",
                getText "token_type", getText "scope") of
            (Just sub, Just at, Just tt, Just scope) ->
              Right TokenSet
                { tsIss          = iss
                , tsSub          = sub
                , tsAud          = ""  -- filled in by verifyIssuer
                , tsScope        = scope
                , tsAccessToken  = at
                , tsTokenType    = tt
                , tsRefreshToken = getText "refresh_token"
                , tsExpiresAt    = fmap (\secs ->
                                     addUTCTime (fromIntegral secs) now)
                                       (getInt "expires_in")
                }
            _ -> Left (OAuthParseError
                   "Token response missing required fields (sub/access_token/token_type/scope)")
    Right _ -> Left (OAuthParseError "Token response is not a JSON object")

-- ---------------------------------------------------------------------------
-- Identity helpers
-- ---------------------------------------------------------------------------

-- | Resolve a handle or DID to a DID.
resolveToDid :: OAuthClient -> T.Text -> IO (Either OAuthError T.Text)
resolveToDid client input
  | "did:" `T.isPrefixOf` input = return (Right input)
  | otherwise = do
      mDid <- occResolveHandle (ocConfig client) input
      case mDid of
        Nothing  -> return (Left (OAuthIdentityError
                              ("Could not resolve handle: " ++ T.unpack input)))
        Just did -> return (Right did)

-- | Resolve a DID to a PDS URL using the caller-provided resolver.
resolvePds :: OAuthClient -> T.Text -> IO (Either OAuthError T.Text)
resolvePds client did = do
  eDoc <- occResolveDid (ocConfig client) did
  case eDoc of
    Left err  -> return (Left (OAuthIdentityError err))
    Right doc ->
      if T.null (ddlPdsUrl doc)
        then return (Left (OAuthNoPdsEndpoint did))
        else return (Right (ddlPdsUrl doc))
