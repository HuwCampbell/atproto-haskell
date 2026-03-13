-- | AT Protocol OAuth CLI demo.
--
-- A minimal command-line application that demonstrates the AT Protocol
-- OAuth 2.1 flow (PKCE, PAR, DPoP) using the atproto-haskell libraries.
--
-- Follows the pattern described at
-- <https://atproto.com/guides/oauth-cli-tutorial>, ported to Haskell.
--
-- Usage:
--
-- @
-- cabal run oauth-cli -- your-handle.bsky.social
-- @
module Main (main) where

import           Control.Concurrent       (MVar, forkIO, newEmptyMVar,
                                           putMVar, takeMVar)
import           Control.Monad            (join)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Types       (queryToQueryText, status200)
import           Network.Wai              (Application, queryString,
                                           responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment       (getArgs)
import           System.Exit              (exitFailure)
import           System.IO                (hFlush, stdout)

import           ATProto.DID              (DidDocument (..), DidResolver (..),
                                           Service (..), defaultPlcResolver,
                                           newWebResolver)
import           ATProto.Identity         (defaultHandleResolverOpts,
                                           newHandleResolver, resolveHandle)
import           ATProto.OAuth            (AuthorizeParams (..),
                                           AuthorizeResult (..),
                                           CallbackParams (..),
                                           Session (..),
                                           TokenSet (..),
                                           authorize, callback,
                                           loopbackClientMetadata,
                                           newInMemorySessionStore,
                                           newInMemoryStateStore,
                                           newOAuthClient,
                                           newOAuthXrpcClient)
import           ATProto.OAuth.Client     (DidDocumentLike (..),
                                           OAuthClientConfig (..),
                                           OAuthClient)
import           ATProto.OAuth.Types      (OAuthClientMetadata (..))
import           ATProto.Repo             (GetProfileParams (..),
                                           ProfileView (..), getProfile)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [handle] -> run (T.pack handle)
    _        -> do
      putStrLn "Usage: oauth-cli <your-handle>"
      putStrLn ""
      putStrLn "Example:"
      putStrLn "  cabal run oauth-cli -- your-handle.bsky.social"
      exitFailure

run :: T.Text -> IO ()
run handle = do
  -- Shared TLS connection manager.
  mgr <- newManager tlsManagerSettings

  -- Identity resolvers.
  handleResolver <- newHandleResolver defaultHandleResolverOpts
  plcRes         <- defaultPlcResolver
  webRes         <- newWebResolver

  -- Start a local callback server on a random available port.
  (port, callbackMVar) <- startCallbackServer
  let callbackUrl = "http://127.0.0.1:" <> T.pack (show port) <> "/callback"

  -- Configure the OAuth client.
  --
  -- For CLI / loopback apps the client_id is "http://localhost" as
  -- specified by the ATProto OAuth loopback client spec.
  let clientMeta = (loopbackClientMetadata "atproto transition:generic" [callbackUrl])
        { cmClientName      = Just "atproto-haskell OAuth CLI demo" }

  stateStore   <- newInMemoryStateStore
  sessionStore <- newInMemorySessionStore

  let cfg = OAuthClientConfig
        { occMetadata           = clientMeta
        , occManager            = mgr
        , occResolveHandle      = resolveHandle handleResolver
        , occResolveDid         = didResolver plcRes webRes
        , occTokenRefreshBuffer = 60
        }

  oauthClient <- newOAuthClient cfg stateStore sessionStore

  -- Start the OAuth flow.
  TIO.putStrLn $ "Logging in as " <> handle <> "..."
  eAuth <- authorize oauthClient AuthorizeParams
    { apInput       = handle
    , apRedirectUri = callbackUrl
    , apScope       = "atproto transition:generic"
    , apAppState    = Nothing
    }

  case eAuth of
    Left err -> do
      putStrLn $ "Authorization error: " ++ show err
      exitFailure
    Right authResult -> do
      TIO.putStrLn ""
      TIO.putStrLn "Please open the following URL in your browser to authorize:"
      TIO.putStrLn ""
      TIO.putStrLn $ "  " <> arRedirectUrl authResult
      TIO.putStrLn ""
      putStr "Waiting for authorization..."
      hFlush stdout

      -- Wait for the callback.
      cp <- takeMVar callbackMVar

      putStrLn " received!"
      TIO.putStrLn ""

      -- Exchange the authorization code for a session.
      eSession <- callback oauthClient cp

      case eSession of
        Left err -> do
          putStrLn $ "Callback error: " ++ show err
          exitFailure
        Right session -> do
          let ts = sessTokenSet session
          TIO.putStrLn $ "Logged in!  DID: " <> tsSub ts
          TIO.putStrLn ""

          -- Make an authenticated getProfile call.
          fetchAndPrintProfile mgr oauthClient session

-- ---------------------------------------------------------------------------
-- Authenticated resource request
-- ---------------------------------------------------------------------------

-- | Fetch and display the authenticated user's profile.
--
-- Uses 'newOAuthXrpcClient' for transparent DPoP proof generation, nonce
-- caching, and token refresh.
fetchAndPrintProfile :: Manager -> OAuthClient -> Session -> IO ()
fetchAndPrintProfile mgr oauthClient session = do
  let did = tsSub (sessTokenSet session)
  authedClient <- newOAuthXrpcClient mgr oauthClient did session
  eProfile     <- getProfile authedClient (GetProfileParams did)
  case eProfile of
    Left err   -> putStrLn $ "getProfile error: " ++ show err
    Right prof -> do
      TIO.putStrLn "Profile:"
      TIO.putStrLn $ "  Handle:       " <> pvHandle prof
      TIO.putStrLn $ "  DID:          " <> pvDid prof
      TIO.putStrLn $ "  Display name: " <> maybe "(not set)" id (pvDisplayName prof)
      TIO.putStrLn $ "  Description:  " <> maybe "(not set)" id (pvDescription prof)
      TIO.putStrLn $ "  Posts:        " <> maybe "?" (T.pack . show) (pvPostsCount prof)
      TIO.putStrLn $ "  Followers:    " <> maybe "?" (T.pack . show) (pvFollowersCount prof)
      TIO.putStrLn $ "  Following:    " <> maybe "?" (T.pack . show) (pvFollowsCount prof)

-- ---------------------------------------------------------------------------
-- Callback server
-- ---------------------------------------------------------------------------

-- | Start a temporary HTTP server on a random available port that listens
-- for the OAuth callback redirect.
--
-- Returns the port number and an 'MVar' that will be filled with the
-- 'CallbackParams' when the callback arrives.
startCallbackServer :: IO (Warp.Port, MVar CallbackParams)
startCallbackServer = do
  mvar <- newEmptyMVar
  (port, sock) <- Warp.openFreePort
  let app :: Application
      app req respond = do
        let params = queryToQueryText (queryString req)
            get k  = fromMaybe "" (join (lookup k params))
        putMVar mvar CallbackParams
          { cpCode  = get "code"
          , cpState = get "state"
          , cpIss   = join (lookup "iss" params)
          }
        respond $ responseLBS status200
          [("Content-Type", "text/html")]
          "<h1>Authorized! You can close this tab.</h1>"
  _ <- forkIO $ Warp.runSettingsSocket Warp.defaultSettings sock app
  return (port, mvar)

-- ---------------------------------------------------------------------------
-- Identity helpers
-- ---------------------------------------------------------------------------

-- | Resolve a DID to a 'DidDocumentLike' using either the PLC or Web
-- resolver depending on the DID method prefix.
didResolver
  :: (DidResolver plc, DidResolver web)
  => plc -> web -> T.Text -> IO (Either String DidDocumentLike)
didResolver plcRes webRes did
  | "did:plc:" `T.isPrefixOf` did = go plcRes
  | "did:web:" `T.isPrefixOf` did = go webRes
  | otherwise = return (Left ("Unsupported DID method: " ++ T.unpack did))
  where
    go resolver = do
      eDoc <- resolve resolver did
      case eDoc of
        Left err  -> return (Left (show err))
        Right doc ->
          case filter isPds (didDocServices doc) of
            (s:_) -> return (Right (DidDocumentLike (serviceEndpoint s)))
            []    -> return (Left ("No PDS service in DID document for " ++ T.unpack did))

    isPds s = serviceType s == "AtprotoPersonalDataServer"
