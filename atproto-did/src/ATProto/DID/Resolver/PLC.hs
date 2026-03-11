-- | Resolver for @did:plc@ identifiers.
--
-- @did:plc@ is the primary DID method in the AT Protocol.  Identifiers look
-- like @did:plc:ewvi7nxzyoun6zhhandbv25b@.  Resolution is a simple HTTP GET
-- to the PLC directory:
--
-- @
-- GET {plcDirectoryUrl}/{did}
-- Accept: application/json
-- @
--
-- The default directory URL is @https://plc.directory@.
--
-- = Usage
--
-- @
-- import ATProto.DID
--
-- main :: IO ()
-- main = do
--   resolver <- defaultPlcResolver
--   result   <- resolve resolver "did:plc:ewvi7nxzyoun6zhhandbv25b"
--   case result of
--     Left  err -> print err
--     Right doc -> print (didDocAlsoKnownAs doc)
-- @
module ATProto.DID.Resolver.PLC
  ( PlcResolver (..)
  , newPlcResolver
  , defaultPlcResolver
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

import ATProto.DID.Document (DidDocument (..))
import ATProto.DID.Resolver (DidResolver (..), ResolveError (..))

-- | A resolver for @did:plc@ identifiers.
data PlcResolver = PlcResolver
  { plcDirectoryUrl :: T.Text
    -- ^ Base URL of the PLC directory, e.g. @\"https://plc.directory\"@.
  , plcManager      :: Manager
    -- ^ Shared TLS-capable HTTP connection manager.
  }

-- | Create a 'PlcResolver' pointing at the given PLC directory URL.
newPlcResolver :: T.Text -> IO PlcResolver
newPlcResolver url = do
  mgr <- newManager tlsManagerSettings
  return (PlcResolver url mgr)

-- | Create a 'PlcResolver' using the canonical AT Protocol PLC directory.
defaultPlcResolver :: IO PlcResolver
defaultPlcResolver = newPlcResolver "https://plc.directory"

instance DidResolver PlcResolver where
  resolve = resolvePlc

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

resolvePlc :: PlcResolver -> T.Text -> IO (Either ResolveError DidDocument)
resolvePlc r did = do
  let url = T.unpack (plcDirectoryUrl r) ++ "/" ++ T.unpack did
  result <- tryIO (fetchJson (plcManager r) url)
  case result of
    Left  err      -> return (Left (DidNetworkError err))
    Right Nothing  -> return (Left (DidNotFound did))
    Right (Just v) ->
      case Aeson.fromJSON v of
        Aeson.Error   msg -> return (Left (DidParseError msg))
        Aeson.Success doc ->
          if didDocId doc == did
            then return (Right doc)
            else return (Left (DidParseError
                   "DID document 'id' does not match the requested DID"))

-- | Fetch a URL and return the JSON body, or 'Nothing' on HTTP 404.
-- Throws on other errors.
fetchJson :: Manager -> String -> IO (Maybe Aeson.Value)
fetchJson mgr url = do
  req  <- addAcceptJson <$> parseRequest url
  resp <- httpLbs req mgr
  let status = statusCode (responseStatus resp)
  case status of
    404 -> return Nothing
    _   | status >= 200 && status < 300 ->
            case Aeson.decode (responseBody resp) of
              Nothing -> fail ("Could not parse JSON response from " ++ url)
              Just v  -> return (Just v)
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
