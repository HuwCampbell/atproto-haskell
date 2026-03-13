-- | Statusphere example app – server-rendered, firehose ingestion, OAuth.
--
-- A Haskell port of the Bluesky "Statusphere" example application.
--
-- Demonstrates:
--
--   * OAuth 2.1 sign-in via @atproto-haskell-oauth@
--   * Writing records to the user's PDS via @com.atproto.repo.putRecord@
--   * Consuming the AT Protocol firehose to ingest @xyz.statusphere.status@ records
--   * SQLite persistence with @sqlite-simple@
--   * Server-rendered HTML with @lucid@
--
-- Usage:
--
-- @
-- cabal run statusphere-og
-- @
--
-- Then open @http://127.0.0.1:3000@ in your browser.
module Main (main) where

import           Control.Concurrent       (forkIO)
import           Control.Exception        (SomeException, catch)
import qualified Data.Aeson               as Aeson
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
import           Lucid                    (Html, renderBS)
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Types       (status200, status302, status400,
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
import           Statusphere.Views

-- ---------------------------------------------------------------------------
-- Application state
-- ---------------------------------------------------------------------------

-- | In-memory session map: DID -> Session.
--
-- Sessions are kept in memory because the OAuth library's Session type
-- (containing a DPoP key) is not directly serialisable.  For a production
-- app you would want to persist DPoP keys as JWKs.
type SessionMap = IORef (Map.Map T.Text Session)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  let port   = 3000
      dbPath = "statusphere.db"

  TIO.putStrLn $ "Starting Statusphere on http://127.0.0.1:" <> T.pack (show port)

  -- Set up shared resources.
  mgr <- newManager tlsManagerSettings
  sessions <- newIORef Map.empty

  -- Initialise the database.
  withDatabase dbPath $ \conn -> do
    migrate conn

    -- Create the OAuth client.
    oauthClient <- createOAuthClient mgr port

    -- Start firehose ingester in background.
    _ <- forkIO $ do
      TIO.putStrLn "Starting firehose ingester..."
      runIngester conn

    -- Start the web server.
    TIO.putStrLn $ "Listening on http://127.0.0.1:" <> T.pack (show port)
    Warp.run port (app conn mgr oauthClient sessions)

-- ---------------------------------------------------------------------------
-- WAI application
-- ---------------------------------------------------------------------------

app :: Connection -> Manager -> OAuthClient -> SessionMap -> Application
app conn mgr oauthClient sessions req respond = do
  let meth = requestMethod req
      path = pathInfo req
  case (meth, path) of
    -- Static assets
    ("GET", ["public", filename]) ->
      serveStatic filename req respond

    -- Home page
    ("GET", []) ->
      handleHome conn req respond

    -- Login page
    ("GET", ["login"]) ->
      handleLoginPage req respond

    -- Login form submission
    ("POST", ["login"]) ->
      handleLogin oauthClient req respond

    -- OAuth callback
    ("GET", ["oauth", "callback"]) ->
      handleCallback oauthClient sessions req respond

    -- Logout
    ("POST", ["logout"]) ->
      handleLogout sessions req respond

    -- Set status
    ("POST", ["status"]) ->
      handleSetStatus conn mgr oauthClient sessions req respond

    -- 404
    _ ->
      respond $ responseLBS status400 [(hContentType, "text/html; charset=utf-8")]
        (renderBS (errorPage "Not found"))

-- ---------------------------------------------------------------------------
-- Route handlers
-- ---------------------------------------------------------------------------

-- | Serve static files from the public/ directory.
serveStatic :: T.Text -> Application
serveStatic filename _req respond = do
  let filepath = "public/" <> T.unpack filename
  exists <- doesFileExist filepath
  if exists
    then respond $ responseFile status200 [(hContentType, contentType filename)] filepath Nothing
    else respond $ responseLBS status400 [(hContentType, "text/plain")] "Not found"
  where
    contentType f
      | ".css" `T.isSuffixOf` f = "text/css; charset=utf-8"
      | ".js"  `T.isSuffixOf` f = "application/javascript"
      | otherwise                = "application/octet-stream"

-- | Home page: show recent statuses.
handleHome :: Connection -> Application
handleHome conn req respond = do
  statuses <- getRecentStatuses conn
  let mDid = getCookie "did" req
  myStatus <- case mDid of
    Just did -> getMyStatus conn did
    Nothing  -> return Nothing
  respond $ htmlResponse (homePage statuses mDid myStatus)

-- | Login page.
handleLoginPage :: Application
handleLoginPage _req respond =
  respond $ htmlResponse (loginPage Nothing)

-- | Handle login form: initiate OAuth flow.
handleLogin :: OAuthClient -> Application
handleLogin oauthClient req respond = do
  body <- strictRequestBody req
  let params = parseFormBody body
      mHandle = Map.lookup "handle" params
  case mHandle of
    Nothing -> respond $ htmlResponse (loginPage (Just "Handle is required"))
    Just handle -> do
      let callbackUrl = "http://127.0.0.1:3000/oauth/callback"
      eAuth <- authorize oauthClient AuthorizeParams
        { apInput       = handle
        , apRedirectUri = callbackUrl
        , apScope       = "atproto transition:generic"
        , apAppState    = Nothing
        }
      case eAuth of
        Left err -> do
          TIO.putStrLn $ "OAuth error: " <> T.pack (show err)
          respond $ htmlResponse (loginPage (Just (T.pack (show err))))
        Right authResult ->
          respond $ redirectResponse (arRedirectUrl authResult) []

-- | OAuth callback: exchange code for session.
handleCallback :: OAuthClient -> SessionMap -> Application
handleCallback oauthClient sessions req respond = do
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
          let did = tsSub (sessTokenSet session)
          -- Store the session in our in-memory map
          modifyIORef' sessions (Map.insert did session)
          TIO.putStrLn $ "Logged in: " <> did
          respond $ redirectResponse "/"
            [("Set-Cookie", TE.encodeUtf8 ("did=" <> did <> "; Path=/; HttpOnly"))]

    _ -> do
      TIO.putStrLn "OAuth callback: missing parameters"
      respond $ redirectResponse "/" []

-- | Handle logout.
handleLogout :: SessionMap -> Application
handleLogout sessions req respond = do
  let mDid = getCookie "did" req
  case mDid of
    Just did -> modifyIORef' sessions (Map.delete did)
    Nothing  -> return ()
  respond $ redirectResponse "/"
    [("Set-Cookie", "did=; Path=/; HttpOnly; Max-Age=0")]

-- | Handle set-status form: write to PDS, then optimistic local insert.
handleSetStatus :: Connection -> Manager -> OAuthClient -> SessionMap -> Application
handleSetStatus conn mgr oauthClient sessions req respond = do
  let mDid = getCookie "did" req
  case mDid of
    Nothing ->
      respond $ responseLBS status401 [(hContentType, "text/html; charset=utf-8")]
        (renderBS (errorPage "Session required"))
    Just did -> do
      body <- strictRequestBody req
      let params = parseFormBody body
          mStatus = Map.lookup "status" params
      case mStatus of
        Nothing ->
          respond $ responseLBS status400 [(hContentType, "text/html; charset=utf-8")]
            (renderBS (errorPage "Invalid status"))
        Just statusEmoji -> do
          -- Look up the in-memory session
          sessionMap <- readIORef sessions
          case Map.lookup did sessionMap of
            Nothing -> do
              respond $ redirectResponse "/" [("Set-Cookie", "did=; Path=/; HttpOnly; Max-Age=0")]
            Just session -> do
              -- Create an authenticated XRPC client
              xrpcClient <- newOAuthXrpcClient mgr oauthClient did session
              -- Generate a TID-like rkey
              rkey <- generateRkey
              now  <- isoNow
              let record = Aeson.object
                    [ "$type"     Aeson..= ("xyz.statusphere.status" :: T.Text)
                    , "status"    Aeson..= statusEmoji
                    , "createdAt" Aeson..= now
                    ]
              -- Write to PDS
              eResult <- putRecord xrpcClient PutRecordRequest
                { prrRepo       = did
                , prrCollection = "xyz.statusphere.status"
                , prrRkey       = rkey
                , prrRecord     = record
                , prrValidate   = Just False
                }
              case eResult of
                Left err -> do
                  TIO.putStrLn $ "putRecord failed: " <> T.pack (show err)
                  respond $ responseLBS status500 [(hContentType, "text/html; charset=utf-8")]
                    (renderBS (errorPage "Failed to write record"))
                Right putResp -> do
                  -- Optimistic local insert
                  safeIO $ insertStatus conn StatusRow
                    { srUri       = prUri putResp
                    , srAuthorDid = did
                    , srStatus    = statusEmoji
                    , srCreatedAt = now
                    , srIndexedAt = now
                    }
                  respond $ redirectResponse "/" []

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Render Lucid HTML to a WAI response.
htmlResponse :: Html () -> Response
htmlResponse h =
  responseLBS status200 [(hContentType, "text/html; charset=utf-8")] (renderBS h)

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
