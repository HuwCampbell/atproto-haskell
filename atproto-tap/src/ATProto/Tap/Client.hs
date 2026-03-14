-- | HTTP client for the Tap admin API.
--
-- Provides bindings for managing tracked repositories, resolving DIDs,
-- and checking Tap's health status.
module ATProto.Tap.Client
  ( -- * Configuration
    TapConfig (..)
    -- * Client
  , TapClient
  , newTapClient
    -- * API operations
  , addRepos
  , removeRepos
  , resolveDid
  , healthCheck
  ) where

import           Control.Exception          (SomeException, catch)
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTPS
import qualified Network.HTTP.Types.Header  as HH
import qualified Network.HTTP.Types.Status  as HS

-- ---------------------------------------------------------------------------
-- Configuration and client
-- ---------------------------------------------------------------------------

-- | Configuration for connecting to a Tap instance.
data TapConfig = TapConfig
  { tcUrl           :: T.Text
    -- ^ Base URL, e.g. @\"http:\/\/localhost:2480\"@.
  , tcAdminPassword :: Maybe T.Text
    -- ^ Optional admin password for Basic auth (@admin:\<password\>@).
  }

-- | An opaque handle wrapping an HTTP manager and configuration.
data TapClient = TapClient
  { _tcConfig  :: TapConfig
  , _tcManager :: HTTP.Manager
  }

-- | Create a new 'TapClient'.  Allocates a shared TLS-capable HTTP manager.
newTapClient :: TapConfig -> IO TapClient
newTapClient cfg = do
  mgr <- HTTP.newManager HTTPS.tlsManagerSettings
  pure TapClient
    { _tcConfig  = cfg
    , _tcManager = mgr
    }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

baseUrl :: TapClient -> String
baseUrl c = T.unpack (tcUrl (_tcConfig c))

authHeader :: TapClient -> [(HH.HeaderName, BS.ByteString)]
authHeader c =
  case tcAdminPassword (_tcConfig c) of
    Nothing  -> []
    Just pwd ->
      let cred = TE.encodeUtf8 ("admin:" <> pwd)
          encoded = encodeBase64 cred
      in  [(HH.hAuthorization, "Basic " <> encoded)]

-- Minimal Base64 encoder for ASCII credentials (avoids extra dependency).
encodeBase64 :: BS.ByteString -> BS.ByteString
encodeBase64 bs =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
      toWord8 i = BS.index alphabet (fromIntegral i)
      bytes = BS.unpack bs
      go [] = []
      go [a] =
        let i0 = fromIntegral a `div` 4
            i1 = (fromIntegral a `mod` 4) * 16
        in [toWord8 i0, toWord8 i1, 0x3D, 0x3D]
      go [a,b] =
        let i0 = fromIntegral a `div` 4
            i1 = (fromIntegral a `mod` 4) * 16 + fromIntegral b `div` 16
            i2 = (fromIntegral b `mod` 16) * 4
        in [toWord8 i0, toWord8 i1, toWord8 i2, 0x3D]
      go (a:b:c:rest) =
        let i0 = fromIntegral a `div` 4
            i1 = (fromIntegral a `mod` 4) * 16 + fromIntegral b `div` 16
            i2 = (fromIntegral b `mod` 16) * 4 + fromIntegral c `div` 64
            i3 = fromIntegral c `mod` 64
        in toWord8 i0 : toWord8 i1 : toWord8 i2 : toWord8 i3 : go rest
  in BS.pack (go bytes)

mkRequest :: TapClient -> String -> String -> IO HTTP.Request
mkRequest client method path' = do
  req <- HTTP.parseRequest (baseUrl client <> path')
  pure req
    { HTTP.method         = TE.encodeUtf8 (T.pack method)
    , HTTP.requestHeaders = authHeader client
                         <> [(HH.hContentType, "application/json")]
    }

-- ---------------------------------------------------------------------------
-- API operations
-- ---------------------------------------------------------------------------

-- | Add repositories to track.
addRepos :: TapClient -> [T.Text] -> IO (Either String ())
addRepos client dids = do
  let action = do
        req <- mkRequest client "POST" "/repos/add"
        let body = Aeson.encode (Aeson.object ["dids" Aeson..= dids])
            req' = req { HTTP.requestBody = HTTP.RequestBodyLBS body }
        resp <- HTTP.httpLbs req' (_tcManager client)
        let status = HS.statusCode (HTTP.responseStatus resp)
        if status >= 200 && status < 300
          then pure (Right ())
          else pure (Left ("addRepos failed with status " <> show status))
  action `catch` handler
  where
    handler :: SomeException -> IO (Either String ())
    handler e = pure (Left (show e))

-- | Remove repositories from tracking.
removeRepos :: TapClient -> [T.Text] -> IO (Either String ())
removeRepos client dids = do
  let action = do
        req <- mkRequest client "POST" "/repos/remove"
        let body = Aeson.encode (Aeson.object ["dids" Aeson..= dids])
            req' = req { HTTP.requestBody = HTTP.RequestBodyLBS body }
        resp <- HTTP.httpLbs req' (_tcManager client)
        let status = HS.statusCode (HTTP.responseStatus resp)
        if status >= 200 && status < 300
          then pure (Right ())
          else pure (Left ("removeRepos failed with status " <> show status))
  action `catch` handler
  where
    handler :: SomeException -> IO (Either String ())
    handler e = pure (Left (show e))

-- | Resolve a DID through Tap.
resolveDid :: TapClient -> T.Text -> IO (Either String Aeson.Value)
resolveDid client did' = do
  let action = do
        req <- mkRequest client "GET" ("/resolve/" <> T.unpack did')
        let req' = req { HTTP.requestBody = mempty }
        resp <- HTTP.httpLbs req' (_tcManager client)
        let status = HS.statusCode (HTTP.responseStatus resp)
        if status >= 200 && status < 300
          then case Aeson.decode (HTTP.responseBody resp) of
                 Just v  -> pure (Right v)
                 Nothing -> pure (Left "Failed to decode response JSON")
          else pure (Left ("resolveDid failed with status " <> show status))
  action `catch` handler
  where
    handler :: SomeException -> IO (Either String Aeson.Value)
    handler e = pure (Left (show e))

-- | Check Tap's health.
healthCheck :: TapClient -> IO (Either String ())
healthCheck client = do
  let action = do
        req <- mkRequest client "GET" "/health"
        let req' = req { HTTP.requestBody = mempty }
        resp <- HTTP.httpLbs req' (_tcManager client)
        let status = HS.statusCode (HTTP.responseStatus resp)
        if status >= 200 && status < 300
          then pure (Right ())
          else pure (Left ("healthCheck failed with status " <> show status))
  action `catch` handler
  where
    handler :: SomeException -> IO (Either String ())
    handler e = pure (Left (show e))
