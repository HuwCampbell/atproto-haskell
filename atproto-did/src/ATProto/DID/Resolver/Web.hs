-- | Resolver for @did:web@ identifiers.
--
-- @did:web@ resolves a DID by performing an HTTP GET to a well-known URL
-- on the host encoded in the identifier.  For @did:web:example.com@ the
-- resolution URL is:
--
-- @
-- GET https://example.com/.well-known/did.json
-- Accept: application/json
-- @
--
-- __AT Protocol restriction__: @did:web@ identifiers with path components
-- (e.g. @did:web:example.com:user:alice@) are not supported by the AT
-- Protocol.  This resolver returns 'DidUnsupported' for such identifiers.
--
-- = Usage
--
-- @
-- import ATProto.DID.Resolver      (resolve)
-- import ATProto.DID.Resolver.Web
--
-- main :: IO ()
-- main = do
--   resolver <- newWebResolver
--   result   <- resolve resolver "did:web:example.com"
--   case result of
--     Left  err -> print err
--     Right doc -> print doc
-- @
module ATProto.DID.Resolver.Web
  ( WebResolver (..)
  , newWebResolver
  ) where

import Control.Exception               (catch, SomeException)
import qualified Data.Aeson            as Aeson
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import Network.HTTP.Client             (Manager, Request, httpLbs,
                                        newManager, parseRequest,
                                        responseBody, responseStatus)
import Network.HTTP.Client.TLS         (tlsManagerSettings)
import Network.HTTP.Types.Status       (statusCode)

import ATProto.DID.Document  (DidDocument)
import ATProto.DID.Resolver  (DidResolver (..), ResolveError (..))

-- | A resolver for @did:web@ identifiers.
data WebResolver = WebResolver
  { webManager :: Manager
    -- ^ Shared TLS-capable HTTP connection manager.
  }

-- | Create a 'WebResolver' with a fresh TLS-capable connection manager.
newWebResolver :: IO WebResolver
newWebResolver = WebResolver <$> newManager tlsManagerSettings

instance DidResolver WebResolver where
  resolve r did = resolveWeb r did

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

resolveWeb :: WebResolver -> T.Text -> IO (Either ResolveError DidDocument)
resolveWeb r did =
  case webDidToUrl did of
    Left  err -> return $ Left err
    Right url -> do
      result <- try' (fetchJson (webManager r) url)
      case result of
        Left  err      -> return $ Left (DidNetworkError err)
        Right Nothing  -> return $ Left (DidNotFound did)
        Right (Just v) ->
          case Aeson.fromJSON v of
            Aeson.Error   msg -> return $ Left (DidParseError msg)
            Aeson.Success doc -> return $ Right doc

-- | Convert a @did:web@ identifier to the resolution URL.
--
-- * @did:web:example.com@ → @https://example.com/.well-known/did.json@
-- * @did:web:example.com:path@ → 'DidUnsupported' (path not supported)
webDidToUrl :: T.Text -> Either ResolveError String
webDidToUrl did =
  case T.stripPrefix "did:web:" did of
    Nothing   -> Left (DidUnsupported did)
    Just rest ->
      if T.elem ':' rest
        then Left $ DidParseError
               "did:web identifiers with path components are not \
               \supported by the AT Protocol"
        else Right ("https://" ++ T.unpack rest ++ "/.well-known/did.json")

-- | Perform an HTTP GET and return the parsed JSON body.
--
-- Returns 'Nothing' on HTTP 4xx, 'Just' the JSON value on 2xx.
-- Throws on network errors.
fetchJson :: Manager -> String -> IO (Maybe Aeson.Value)
fetchJson mgr url = do
  req <- addAcceptJson <$> parseRequest url
  resp <- httpLbs req mgr
  let status = statusCode (responseStatus resp)
  case status of
    _ | status >= 200 && status < 300 ->
          case Aeson.decode (responseBody resp) of
            Nothing -> fail ("Could not parse JSON from " ++ url)
            Just v  -> return (Just v)
      | status >= 400 && status < 500 ->
          return Nothing
      | otherwise ->
          fail ("HTTP " ++ show status ++ " from " ++ url)

-- | Add @Accept: application/json@ to a request.
addAcceptJson :: Request -> Request
addAcceptJson req =
  req { Network.HTTP.Client.requestHeaders =
          (TE.encodeUtf8 "Accept", "application/json")
          : Network.HTTP.Client.requestHeaders req
      }

-- | Catch all synchronous exceptions and return them as 'Left String'.
try' :: IO a -> IO (Either String a)
try' action = catch (fmap Right action) handler
  where
    handler :: SomeException -> IO (Either String a)
    handler e = return $ Left (show e)
