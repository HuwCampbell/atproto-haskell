-- | Statusphere SPA example – Elm frontend, Haskell XRPC backend, Tap sync.
--
-- A single-page application version of the Statusphere example.  The Elm
-- frontend communicates with the Haskell backend via two custom XRPC
-- endpoints.  The backend uses Tap (rather than the raw firehose) for
-- synchronisation.
--
-- = Architecture
--
-- * __Frontend__ (Elm): serves from @public/@, talks to the backend via
--   JSON XRPC calls.
-- * __Backend__ (Haskell/Warp): provides two XRPC endpoints plus OAuth
--   routes, and serves static files.
--
--     * @GET  \/xrpc\/xyz.statusphere.getStatuses@ – returns recent statuses
--     * @POST \/xrpc\/xyz.statusphere.setStatus@   – sets the user's status
--     * OAuth login\/callback\/logout
--
-- * __Sync__: A background thread connects to a Tap instance to ingest
--   @xyz.statusphere.status@ records.
--
-- = Usage
--
-- @
-- cabal run statusphere-spa -- [--tap-host HOST] [--tap-port PORT]
-- @
--
-- Then open @http://127.0.0.1:3001@ in your browser.
module Main (main) where

import           Control.Concurrent       (forkIO)
import           Control.Exception        (SomeException, catch)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL
import           Data.IORef               (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.IO             as TIO
import           Data.Time.Clock          (getCurrentTime)
import           Data.Time.Format         (formatTime, defaultTimeLocale)
import           Database.SQLite.Simple   (Connection)
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Types       (Status, status200, status302, status400,
                                           status401, status500,
                                           hContentType, hLocation, hCookie,
                                           urlDecode, queryToQueryText,
                                           Header)
import           Network.Wai              (Application, Request, Response,
                                           queryString, requestMethod,
                                           pathInfo, responseLBS,
                                           responseFile, strictRequestBody,
                                           requestHeaders)
import qualified Network.Wai.Handler.Warp as Warp
import           System.Directory         (doesFileExist)
import           System.Environment       (getArgs)
import           Web.ClientSession        (Key, getDefaultKey, encryptIO, decrypt)

import qualified ATProto.Lex.Codec        as Codec
import qualified ATProto.Lex.Json         as LexJson
import           ATProto.OAuth            (AuthorizeParams (..),
                                           AuthorizeResult (..),
                                           CallbackParams (..),
                                           Session (..),
                                           TokenSet (..),
                                           authorize, callback,
                                           newOAuthXrpcClient)
import           ATProto.OAuth.Client     (OAuthClient)
import           ATProto.Repo             (PutRecordRequest (..),
                                           PutRecordResponse (..),
                                           putRecord)

import           Statusphere.Auth         (createOAuthClient)
import           Statusphere.Database
import           Statusphere.Ingester     (runIngester)
import           Statusphere.Types

-- ---------------------------------------------------------------------------
-- Application state
-- ---------------------------------------------------------------------------

-- | In-memory session map: DID -> Session.
type SessionMap = IORef (Map.Map T.Text Session)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let (tapHost, tapPort) = parseArgs args "localhost" 2480
      port   = 3001
      dbPath = "statusphere-spa.db"

  TIO.putStrLn $ "Starting Statusphere SPA on http://127.0.0.1:" <> T.pack (show port)

  -- Shared resources.
  mgr      <- newManager tlsManagerSettings
  sessions <- newIORef Map.empty

  -- Load or create the clientsession encryption key.
  sessionKey <- getDefaultKey

  -- Initialise the database.
  withDatabase dbPath $ \conn -> do
    migrate conn

    -- Create the OAuth client.
    oauthClient <- createOAuthClient mgr port

    -- Start Tap ingester in background.
    _ <- forkIO $ do
      TIO.putStrLn $ "Starting Tap ingester (ws://" <> T.pack tapHost
                    <> ":" <> T.pack (show tapPort) <> "/channel)..."
      runIngester conn (T.pack tapHost) tapPort

    -- Start the web server.
    TIO.putStrLn $ "Listening on http://127.0.0.1:" <> T.pack (show port)
    Warp.run port (app conn mgr oauthClient sessions port sessionKey)

-- | Parse command line arguments for tap host/port.
parseArgs :: [String] -> String -> Int -> (String, Int)
parseArgs [] h p = (h, p)
parseArgs ("--tap-host" : host : rest) _ p = parseArgs rest host p
parseArgs ("--tap-port" : port' : rest) h _ = parseArgs rest h (read port')
parseArgs (_ : rest) h p = parseArgs rest h p

-- ---------------------------------------------------------------------------
-- WAI application
-- ---------------------------------------------------------------------------

app :: Connection -> Manager -> OAuthClient -> SessionMap -> Int -> Key -> Application
app conn mgr oauthClient sessions port sessionKey req respond = do
  let meth = requestMethod req
      path = pathInfo req
  case (meth, path) of
    -- XRPC endpoints
    ("GET", ["xrpc", "xyz.statusphere.getStatuses"]) ->
      handleGetStatuses conn req respond

    ("POST", ["xrpc", "xyz.statusphere.setStatus"]) ->
      handleSetStatus conn mgr oauthClient sessions sessionKey req respond

    -- Session info
    ("GET", ["xrpc", "xyz.statusphere.getSession"]) ->
      handleGetSession sessionKey req respond

    -- OAuth routes
    ("GET", ["login"]) ->
      handleLoginGet oauthClient port req respond

    ("POST", ["login"]) ->
      handleLogin oauthClient port req respond

    ("GET", ["oauth", "callback"]) ->
      handleCallback oauthClient sessions sessionKey req respond

    ("POST", ["logout"]) ->
      handleLogout sessions sessionKey req respond

    -- Static files
    ("GET", []) ->
      serveFile "public/index.html" "text/html; charset=utf-8" respond

    ("GET", ["public", filename]) ->
      serveStatic filename respond

    -- 404
    _ ->
      respond $ jsonError status400 "NotFound" "Route not found"

-- ---------------------------------------------------------------------------
-- XRPC: xyz.statusphere.getStatuses
-- ---------------------------------------------------------------------------

-- | Return the most recent statuses as JSON.
handleGetStatuses :: Connection -> Application
handleGetStatuses conn _req respond = do
  rows <- getRecentStatuses conn
  let entries = map rowToEntry rows
      resp = GetStatusesResponse entries
  respond $ responseLBS status200
    [(hContentType, "application/json"), corsHeaders]
    (LexJson.encode getStatusesResponseCodec resp)

-- ---------------------------------------------------------------------------
-- XRPC: xyz.statusphere.setStatus
-- ---------------------------------------------------------------------------

-- | Set the user's status: writes to PDS, then optimistically inserts.
handleSetStatus :: Connection -> Manager -> OAuthClient -> SessionMap -> Key -> Application
handleSetStatus conn mgr oauthClient sessions sessionKey req respond = do
  let mDid = getSessionDid sessionKey req
  case mDid of
    Nothing ->
      respond $ jsonError status401 "AuthRequired" "Login required"
    Just did' -> do
      body <- strictRequestBody req
      case LexJson.decode setStatusRequestCodec body of
        Left err ->
          respond $ jsonError status400 "InvalidRequest" (T.pack err)
        Right setReq -> do
          sessionMap <- readIORef sessions
          case Map.lookup did' sessionMap of
            Nothing ->
              respond $ jsonError status401 "SessionExpired" "Session not found"
            Just session -> do
              -- Create an authenticated XRPC client
              xrpcClient <- newOAuthXrpcClient mgr oauthClient did' session
              -- Generate a TID-like rkey
              rkey <- generateRkey
              now  <- isoNow
              let record = StatusView (ssrStatus setReq) now
              -- Write to PDS
              eResult <- putRecord xrpcClient PutRecordRequest
                { prrRepo       = did'
                , prrCollection = "xyz.statusphere.status"
                , prrRkey       = rkey
                , prrRecord     = Codec.writer statusViewCodec record
                , prrValidate   = Just False
                }
              case eResult of
                Left err -> do
                  TIO.putStrLn $ "putRecord failed: " <> T.pack (show err)
                  respond $ jsonError status500 "WriteError" "Failed to write record"
                Right putResp -> do
                  -- Optimistic local insert
                  safeIO $ insertStatus conn StatusRow
                    { srUri       = prUri putResp
                    , srAuthorDid = did'
                    , srStatus    = ssrStatus setReq
                    , srCreatedAt = now
                    , srIndexedAt = now
                    }
                  let resp = SetStatusResponse (prUri putResp) (ssrStatus setReq)
                  respond $ responseLBS status200
                    [(hContentType, "application/json"), corsHeaders]
                    (LexJson.encode setStatusResponseCodec resp)

-- ---------------------------------------------------------------------------
-- Session info
-- ---------------------------------------------------------------------------

-- | Return the current user's DID if logged in.
handleGetSession :: Key -> Application
handleGetSession sessionKey req respond = do
  let mDid = getSessionDid sessionKey req
  case mDid of
    Nothing ->
      respond $ responseLBS status200
        [(hContentType, "application/json"), corsHeaders]
        "{\"did\":null}"
    Just did' ->
      respond $ responseLBS status200
        [(hContentType, "application/json"), corsHeaders]
        (BL.fromStrict (TE.encodeUtf8 ("{\"did\":\"" <> did' <> "\"}")))

-- ---------------------------------------------------------------------------
-- OAuth routes
-- ---------------------------------------------------------------------------

-- | Initiate OAuth login flow from a GET request (Elm uses Nav.load).
handleLoginGet :: OAuthClient -> Int -> Application
handleLoginGet oauthClient port req respond = do
  let qs = queryToQueryText (queryString req)
      mHandle = lookup "handle" qs >>= id
  case mHandle of
    Nothing ->
      respond $ jsonError status400 "InvalidRequest" "Handle is required"
    Just handle' ->
      startOAuth oauthClient port handle' respond

-- | Initiate OAuth login flow from a POST request (form-encoded).
handleLogin :: OAuthClient -> Int -> Application
handleLogin oauthClient port req respond = do
  body <- strictRequestBody req
  let params = parseFormBody body
      mHandle = Map.lookup "handle" params
  case mHandle of
    Nothing ->
      respond $ jsonError status400 "InvalidRequest" "Handle is required"
    Just handle' ->
      startOAuth oauthClient port handle' respond

-- | Shared OAuth authorize logic.
startOAuth :: OAuthClient -> Int -> T.Text -> (Response -> IO b) -> IO b
startOAuth oauthClient port handle' respond = do
  let callbackUrl = "http://127.0.0.1:" <> T.pack (show port) <> "/oauth/callback"
  eAuth <- authorize oauthClient AuthorizeParams
    { apInput       = handle'
    , apRedirectUri = callbackUrl
    , apScope       = "atproto transition:generic"
    , apAppState    = Nothing
    }
  case eAuth of
    Left err -> do
      TIO.putStrLn $ "OAuth error: " <> T.pack (show err)
      respond $ jsonError status500 "OAuthError" (T.pack (show err))
    Right authResult ->
      respond $ redirectResponse (arRedirectUrl authResult) []

-- | OAuth callback: exchange code for session.
handleCallback :: OAuthClient -> SessionMap -> Key -> Application
handleCallback oauthClient sessions sessionKey req respond = do
  let qs = queryToQueryText (queryString req)
      mCode  = lookup "code"  qs >>= id
      mState = lookup "state" qs >>= id
      mIss   = lookup "iss"   qs >>= id
  case (mCode, mState, mIss) of
    (Just code, Just state, Just iss) -> do
      eSession <- callback oauthClient CallbackParams
        { cpCode  = code
        , cpState = state
        , cpIss   = Just iss
        }
      case eSession of
        Left err -> do
          TIO.putStrLn $ "OAuth callback error: " <> T.pack (show err)
          respond $ redirectResponse "/" []
        Right session -> do
          let did' = tsSub (sessTokenSet session)
          modifyIORef' sessions (Map.insert did' session)
          TIO.putStrLn $ "Logged in: " <> did'
          encrypted <- encryptIO sessionKey (TE.encodeUtf8 did')
          let cookieVal = TE.decodeUtf8Lenient encrypted
          respond $ redirectResponse "/"
            [("Set-Cookie", TE.encodeUtf8 ("session=" <> cookieVal <> "; Path=/; HttpOnly; SameSite=Lax"))]

    _ -> do
      TIO.putStrLn "OAuth callback: missing parameters"
      respond $ redirectResponse "/" []

-- | Logout.
handleLogout :: SessionMap -> Key -> Application
handleLogout sessions sessionKey req respond = do
  let mDid = getSessionDid sessionKey req
  case mDid of
    Just did' -> modifyIORef' sessions (Map.delete did')
    Nothing   -> return ()
  respond $ redirectResponse "/"
    [("Set-Cookie", "session=; Path=/; HttpOnly; Max-Age=0")]

-- ---------------------------------------------------------------------------
-- Static files
-- ---------------------------------------------------------------------------

serveFile :: FilePath -> BS.ByteString -> (Response -> IO b) -> IO b
serveFile filepath ct respond = do
  exists <- doesFileExist filepath
  if exists
    then respond $ responseFile status200 [(hContentType, ct)] filepath Nothing
    else respond $ responseLBS status400 [(hContentType, "text/plain")] "Not found"

serveStatic :: T.Text -> (Response -> IO b) -> IO b
serveStatic filename respond = do
  let filepath = "public/" <> T.unpack filename
  exists <- doesFileExist filepath
  if exists
    then respond $ responseFile status200 [(hContentType, contentType filename)] filepath Nothing
    else respond $ responseLBS status400 [(hContentType, "text/plain")] "Not found"
  where
    contentType f
      | ".css"  `T.isSuffixOf` f = "text/css; charset=utf-8"
      | ".js"   `T.isSuffixOf` f = "application/javascript; charset=utf-8"
      | ".html" `T.isSuffixOf` f = "text/html; charset=utf-8"
      | otherwise                = "application/octet-stream"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Build a JSON error response.
jsonError :: Status -> T.Text -> T.Text -> Response
jsonError status errCode msg =
  responseLBS status
    [(hContentType, "application/json"), corsHeaders]
    (BL.fromStrict $ TE.encodeUtf8 $
      "{\"error\":\"" <> errCode <> "\",\"message\":\"" <> msg <> "\"}")

-- | CORS header for local development.
corsHeaders :: Header
corsHeaders = ("Access-Control-Allow-Origin", "*")

-- | Convert a StatusRow to a StatusEntry.
rowToEntry :: StatusRow -> StatusEntry
rowToEntry r = StatusEntry
  { seUri       = srUri r
  , seAuthorDid = srAuthorDid r
  , seStatus    = srStatus r
  , seCreatedAt = srCreatedAt r
  , seIndexedAt = srIndexedAt r
  }

-- | Redirect response.
redirectResponse :: T.Text -> [Header] -> Response
redirectResponse url extraHeaders =
  responseLBS status302
    ((hLocation, TE.encodeUtf8 url) : extraHeaders)
    ""

-- | Parse form-urlencoded body into a map.
parseFormBody :: BL.ByteString -> Map.Map T.Text T.Text
parseFormBody body =
  let bs = BL.toStrict body
      pairs = map (T.breakOn "=") $ T.splitOn "&" (TE.decodeUtf8Lenient bs)
  in  Map.fromList
        [ (k, TE.decodeUtf8Lenient (urlDecode True (TE.encodeUtf8 (T.drop 1 v))))
        | (k, v) <- pairs
        , not (T.null k)
        ]

-- | Extract a cookie value from a request.
getCookie :: T.Text -> Request -> Maybe T.Text
getCookie name req =
  let headers = map snd $ filter (\(k, _) -> k == hCookie) (requestHeaders req)
      cookies = concatMap parseCookies headers
  in  lookup name cookies
  where
    parseCookies :: BS.ByteString -> [(T.Text, T.Text)]
    parseCookies bs =
      let txt = TE.decodeUtf8Lenient bs
          pairs = T.splitOn "; " txt
      in  [ (T.strip k, T.drop 1 v)
          | p <- pairs
          , let (k, v) = T.breakOn "=" p
          , not (T.null v)
          ]

-- | Decrypt the session cookie and return the DID.
getSessionDid :: Key -> Request -> Maybe T.Text
getSessionDid key req =
  case getCookie "session" req of
    Nothing  -> Nothing
    Just raw ->
      case decrypt key (TE.encodeUtf8 raw) of
        Nothing -> Nothing
        Just bs -> Just (TE.decodeUtf8Lenient bs)

-- | Generate a TID-like record key (microsecond timestamp).
generateRkey :: IO T.Text
generateRkey = do
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%s%q" now
  return $ T.pack (take 13 ts)

-- | Get the current time as ISO 8601 string.
isoNow :: IO T.Text
isoNow = do
  now <- getCurrentTime
  return $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000Z" now

-- | Catch and ignore any IO exception.
safeIO :: IO () -> IO ()
safeIO action = action `catch` handler
  where
    handler :: SomeException -> IO ()
    handler _ = return ()
