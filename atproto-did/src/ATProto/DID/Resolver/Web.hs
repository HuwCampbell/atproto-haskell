-- | Resolver for @did:web@ identifiers.
--
-- @did:web@ resolves a DID by fetching a well-known JSON document from the
-- host encoded in the identifier.  For @did:web:example.com@ the URL is:
--
-- @
-- GET https://example.com/.well-known/did.json
-- Accept: application/json
-- @
--
-- __AT Protocol restriction__: @did:web@ identifiers with path components
-- (e.g. @did:web:example.com:user:alice@) are not supported.  This resolver
-- returns 'DidParseError' for such inputs.
--
-- = Usage
--
-- @
-- import ATProto.DID
--
-- main :: IO ()
-- main = do
--   resolver <- newWebResolver
--   result   <- resolve resolver "did:web:example.com"
--   case result of
--     Left  err -> print err
--     Right doc -> print (didDocServices doc)
-- @
module ATProto.DID.Resolver.Web
  ( WebResolver (..)
  , newWebResolver
  ) where

import           Control.Exception         (SomeException, catch)
import qualified Data.Aeson                as Aeson
import qualified Data.Text                 as T
import           Network.HTTP.Client       (Manager, Request, httpLbs,
                                            newManager, parseRequest,
                                            requestHeaders, responseBody,
                                            responseStatus)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)

import ATProto.DID.Document (DidDocument)
import ATProto.DID.Resolver (DidResolver (..), ResolveError (..))

-- | A resolver for @did:web@ identifiers.
data WebResolver = WebResolver
  { webManager :: Manager
    -- ^ Shared TLS-capable HTTP connection manager.
  }

-- | Create a 'WebResolver' with a fresh TLS-capable connection manager.
newWebResolver :: IO WebResolver
newWebResolver = WebResolver <$> newManager tlsManagerSettings

instance DidResolver WebResolver where
  resolve = resolveWeb

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

resolveWeb :: WebResolver -> T.Text -> IO (Either ResolveError DidDocument)
resolveWeb r did =
  case webDidToUrl did of
    Left  err -> return (Left err)
    Right url -> do
      result <- tryIO (fetchJson (webManager r) url)
      case result of
        Left  err      -> return (Left (DidNetworkError err))
        Right Nothing  -> return (Left (DidNotFound did))
        Right (Just v) ->
          case Aeson.fromJSON v of
            Aeson.Error   msg -> return (Left (DidParseError msg))
            Aeson.Success doc -> return (Right doc)

-- | Convert a @did:web@ DID to its resolution URL.
--
-- Returns 'Left' if the DID has path components (unsupported in AT Protocol)
-- or is not a @did:web@ DID.
webDidToUrl :: T.Text -> Either ResolveError String
webDidToUrl did =
  case T.stripPrefix "did:web:" did of
    Nothing   -> Left (DidUnsupported did)
    Just host ->
      if T.elem ':' host
        then Left (DidParseError
               "did:web with path components is not supported by the AT Protocol")
        else Right ("https://" ++ T.unpack host ++ "/.well-known/did.json")

-- | Fetch a URL, return 'Nothing' on 4xx, parsed JSON on 2xx.
fetchJson :: Manager -> String -> IO (Maybe Aeson.Value)
fetchJson mgr url = do
  req  <- addAcceptJson <$> parseRequest url
  resp <- httpLbs req mgr
  let status = statusCode (responseStatus resp)
  case status of
    _ | status >= 200 && status < 300 ->
            case Aeson.decode (responseBody resp) of
              Nothing -> fail ("Could not parse JSON response from " ++ url)
              Just v  -> return (Just v)
      | status >= 400 && status < 500 -> return Nothing
      | otherwise ->
            fail ("HTTP " ++ show status ++ " from " ++ url)

addAcceptJson :: Request -> Request
addAcceptJson req =
  req { requestHeaders = ("Accept", "application/json")
                         : requestHeaders req }

tryIO :: IO a -> IO (Either String a)
tryIO action = catch (fmap Right action) handler
  where
    handler :: SomeException -> IO (Either String a)
    handler e = return (Left (show e))
