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
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Data.Maybe               (fromMaybe)
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
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
                                           DpopClaims (..), DpopKey,
                                           Session (..),
                                           TokenSet (..), accessTokenHash,
                                           authorize, callback,
                                           createDpopProof,
                                           defaultClientMetadata,
                                           newInMemorySessionStore,
                                           newInMemoryStateStore,
                                           newOAuthClient)
import           ATProto.OAuth.Client     (DidDocumentLike (..),
                                           OAuthClientConfig (..))
import           ATProto.OAuth.Types      (OAuthClientMetadata (..))
import           ATProto.Repo             (GetProfileParams (..),
                                           ProfileView (..), getProfile)
import           ATProto.XRPC             (XrpcClient (..), XrpcError (..),
                                           XrpcMethod (..), XrpcRequest (..),
                                           XrpcResponse (..))
import           ATProto.XRPC.Http        (HttpXrpcClient,
                                           newHttpXrpcClientWith)

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
  let callbackUrl = "http://127.0.0.1:" <> T.pack (show port) <> "/"

  -- Configure the OAuth client.
  --
  -- For CLI / loopback apps the client_id is "http://localhost" as
  -- specified by the ATProto OAuth loopback client spec.
  let clientMeta = (defaultClientMetadata "http://localhost" [callbackUrl])
        { cmClientName      = Just "atproto-haskell OAuth CLI demo"
        , cmApplicationType = Just "native"
        }

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
    , apScope       = "atproto" --  transition:generic"
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
          fetchAndPrintProfile mgr session

-- ---------------------------------------------------------------------------
-- Authenticated resource request
-- ---------------------------------------------------------------------------

-- | Fetch and display the authenticated user's profile.
--
-- Constructs a DPoP-protected XRPC request using the session's ephemeral
-- key and access token.  Handles @use_dpop_nonce@ retries automatically.
fetchAndPrintProfile :: Manager -> Session -> IO ()
fetchAndPrintProfile mgr session = do
  let ts     = sessTokenSet session
      pdsUrl = tsAud ts
      did    = tsSub ts

  authedClient <- newDpopXrpcClient mgr pdsUrl (sessDpopKey session) (tsAccessToken ts)
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

-- | A wrapper around 'HttpXrpcClient' that attaches DPoP proofs and
-- Authorization headers to every request, and retries once on
-- @use_dpop_nonce@ errors.
data DpopXrpcClient = DpopXrpcClient
  { dxcInner       :: HttpXrpcClient
  , dxcPdsUrl      :: T.Text
  , dxcDpopKey     :: DpopKey
  , dxcAccessToken :: T.Text
  , dxcNonceRef    :: IORef (Maybe T.Text)
    -- ^ Cached DPoP nonce for this PDS, updated from response headers.
  }

-- | Create a new 'DpopXrpcClient'.
newDpopXrpcClient :: Manager -> T.Text -> DpopKey -> T.Text -> IO DpopXrpcClient
newDpopXrpcClient mgr pdsUrl dpopKey accessToken = do
  nonceRef <- newIORef Nothing
  return DpopXrpcClient
    { dxcInner       = newHttpXrpcClientWith mgr pdsUrl
    , dxcPdsUrl      = pdsUrl
    , dxcDpopKey     = dpopKey
    , dxcAccessToken = accessToken
    , dxcNonceRef    = nonceRef
    }

instance XrpcClient DpopXrpcClient where
  runXrpc client req = do
    result <- runDpopXrpc client req
    case result of
      Right resp -> return (Right resp)
      Left err
        | xrpcErrError err == "use_dpop_nonce" -> do
            -- Extract the nonce from the error response and retry once.
            let mNonce = Map.lookup "DPoP-Nonce" (xrpcErrHeaders err)
            case mNonce of
              Nothing    -> return (Left err)
              Just nonce -> do
                writeIORef (dxcNonceRef client) (Just nonce)
                runDpopXrpc client req
        | otherwise -> return (Left err)

-- | Execute one DPoP-protected XRPC request (no retry).
runDpopXrpc :: DpopXrpcClient -> XrpcRequest -> IO (Either XrpcError XrpcResponse)
runDpopXrpc client req = do
  cachedNonce <- readIORef (dxcNonceRef client)
  let httpMethod = case xrpcReqMethod req of
                     XrpcQuery     -> "GET"
                     XrpcProcedure -> "POST"
      resourceUrl = dxcPdsUrl client <> "/xrpc/" <> xrpcReqNsid req
      ath = TE.decodeUtf8Lenient (accessTokenHash (dxcAccessToken client))
  eProof <- createDpopProof (dxcDpopKey client) DpopClaims
    { dcHtm   = httpMethod
    , dcHtu   = resourceUrl
    , dcNonce = cachedNonce
    , dcAth   = Just ath
    }
  case eProof of
    Left err -> return $ Left XrpcError
      { xrpcErrError   = "DpopProofError"
      , xrpcErrMessage = Just (T.pack err)
      , xrpcErrStatus  = 0
      , xrpcErrHeaders = Map.empty
      }
    Right proof -> do
      let hdrs = Map.union
                   (Map.fromList
                     [ ("DPoP",          proof)
                     , ("Authorization", "DPoP " <> dxcAccessToken client)
                     ])
                   (xrpcReqHeaders req)
      result <- runXrpc (dxcInner client) req { xrpcReqHeaders = hdrs }
      -- Update the nonce cache from the response headers (success or error).
      let respHeaders = case result of
                          Right resp -> xrpcRespHeaders resp
                          Left  err  -> xrpcErrHeaders err
      case Map.lookup "DPoP-Nonce" respHeaders of
        Just nonce -> writeIORef (dxcNonceRef client) (Just nonce)
        Nothing    -> return ()
      return result

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
