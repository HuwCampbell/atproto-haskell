-- | Authorization server and protected resource metadata fetching.
--
-- Implements the server discovery flow for the ATProto OAuth profile:
--
-- 1. Fetch protected resource metadata (RFC 9728) from the user's PDS at
--    @\/.well-known\/oauth-protected-resource@.  This document identifies
--    the authorization server (issuer URL).
--
-- 2. Fetch authorization server metadata (RFC 8414) from the issuer at
--    @\/.well-known\/oauth-authorization-server@.  This document provides
--    all endpoint URLs needed for the OAuth flow.
--
-- Both endpoints return JSON and are fetched over HTTPS.
module ATProto.OAuth.ServerMetadata
  ( -- * Discovery
    fetchProtectedResourceMetadata
  , fetchAuthorizationServerMetadata
  , getIssuerForPds
    -- * URL helpers (exported for testing)
  , prmUrl
  , asmUrl
  ) where

import           Control.Exception              (SomeException, catch)
import qualified Data.Aeson                     as Aeson
import qualified Data.Aeson.Types               as AesonTypes
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text                      as T
import           Network.HTTP.Client            (Manager, Request, httpLbs,
                                                 parseRequest, requestHeaders,
                                                 responseBody, responseStatus)
import           Network.HTTP.Types.Status      (statusCode)

import           ATProto.OAuth.Types

-- ---------------------------------------------------------------------------
-- Discovery
-- ---------------------------------------------------------------------------

-- | Fetch the protected resource metadata for a PDS endpoint.
--
-- The PDS is expected to serve a JSON document at
-- @\/.well-known\/oauth-protected-resource@ following RFC 9728.
--
-- Returns 'OAuthNoIssuer' when the metadata document contains no
-- @authorization_servers@ entries.
fetchProtectedResourceMetadata
  :: Manager
  -> T.Text   -- ^ PDS base URL (e.g. @\"https://bsky.social\"@)
  -> IO (Either OAuthError OAuthProtectedResourceMetadata)
fetchProtectedResourceMetadata mgr pdsUrl =
  fetchJson mgr (prmUrl pdsUrl)

-- | Fetch the authorization server metadata for an issuer.
--
-- The issuer is expected to serve a JSON document at
-- @\/.well-known\/oauth-authorization-server@ following RFC 8414.
fetchAuthorizationServerMetadata
  :: Manager
  -> T.Text   -- ^ Issuer URL (e.g. @\"https://bsky.social\"@)
  -> IO (Either OAuthError OAuthAuthorizationServerMetadata)
fetchAuthorizationServerMetadata mgr issuerUrl =
  fetchJson mgr (asmUrl issuerUrl)

-- | Determine the issuer URL for a given PDS by fetching its protected
-- resource metadata.
--
-- Returns the first entry of @authorization_servers@, or 'OAuthNoIssuer'
-- when the list is empty.
getIssuerForPds
  :: Manager
  -> T.Text   -- ^ PDS base URL
  -> IO (Either OAuthError T.Text)
getIssuerForPds mgr pdsUrl = do
  result <- fetchProtectedResourceMetadata mgr pdsUrl
  case result of
    Left  err  -> return (Left err)
    Right prm  ->
      case prmAuthorizationServers prm of
        []         -> return (Left (OAuthNoIssuer pdsUrl))
        (issuer:_) -> return (Right issuer)

-- ---------------------------------------------------------------------------
-- URL helpers
-- ---------------------------------------------------------------------------

-- | Build the protected resource metadata URL for a PDS.
prmUrl :: T.Text -> String
prmUrl pdsUrl =
  T.unpack (T.dropWhileEnd (== '/') pdsUrl)
  ++ "/.well-known/oauth-protected-resource"

-- | Build the authorization server metadata URL for an issuer.
asmUrl :: T.Text -> String
asmUrl issuerUrl =
  T.unpack (T.dropWhileEnd (== '/') issuerUrl)
  ++ "/.well-known/oauth-authorization-server"

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Fetch a URL and decode the response body as JSON.
fetchJson
  :: Aeson.FromJSON a
  => Manager
  -> String
  -> IO (Either OAuthError a)
fetchJson mgr url =
  catch go (\e -> return (Left (OAuthNetworkError (show (e :: SomeException)))))
  where
    go :: Aeson.FromJSON a => IO (Either OAuthError a)
    go = do
      req  <- addAcceptJson <$> parseRequest url
      resp <- httpLbs req mgr
      let status = statusCode (responseStatus resp)
          body   = responseBody resp
      if status >= 200 && status < 300
        then decodeBody url body
        else return (Left (parseServerError status body))

-- | Parse the JSON response body into the target type.
decodeBody
  :: Aeson.FromJSON a
  => String
  -> BL.ByteString
  -> IO (Either OAuthError a)
decodeBody url body =
  case Aeson.eitherDecode body of
    Right v  -> return (Right v)
    Left msg -> return (Left (OAuthParseError
                  ("Could not parse response from " ++ url ++ ": " ++ msg)))

-- | Parse an OAuth error response from a non-2xx body.
parseServerError :: Int -> BL.ByteString -> OAuthError
parseServerError status body =
  case (Aeson.eitherDecode body :: Either String Aeson.Value) of
    Right (Aeson.Object o) ->
      let get k = case AesonTypes.parseMaybe (Aeson..: k) o of
                    Just (Aeson.String t) -> Just t
                    _                     -> Nothing
          errToken = maybe "Error" id (get "error")
          errMsg   = get "message"
      in  OAuthServerError errToken errMsg status
    _ -> OAuthServerError "Error" Nothing status

-- | Add the @Accept: application/json@ header to a request.
addAcceptJson :: Request -> Request
addAcceptJson req =
  req { requestHeaders = ("Accept", "application/json") : requestHeaders req }
