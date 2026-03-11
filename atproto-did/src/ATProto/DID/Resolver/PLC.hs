-- | Resolver for @did:plc@ identifiers.
--
-- @did:plc@ is the primary DID method in the AT Protocol.  Identifiers look
-- like @did:plc:ewvi7nxzyoun6zhhandbv25b@.  Resolution is performed by a
-- simple HTTP GET to the PLC directory:
--
-- @
-- GET {plcDirectoryUrl}/{did}
-- Accept: application/json
-- @
--
-- The default directory is @https://plc.directory@.
--
-- = Usage
--
-- @
-- import ATProto.DID.Resolver      (resolve)
-- import ATProto.DID.Resolver.PLC
--
-- main :: IO ()
-- main = do
--   resolver <- defaultPlcResolver
--   result   <- resolve resolver "did:plc:ewvi7nxzyoun6zhhandbv25b"
--   case result of
--     Left  err -> print err
--     Right doc -> print doc
-- @
module ATProto.DID.Resolver.PLC
  ( PlcResolver (..)
  , newPlcResolver
  , defaultPlcResolver
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

-- | A resolver for @did:plc@ identifiers.
data PlcResolver = PlcResolver
  { plcDirectoryUrl :: T.Text
    -- ^ Base URL of the PLC directory, e.g. @\"https://plc.directory\"@.
  , plcManager      :: Manager
    -- ^ Shared TLS-capable HTTP connection manager.
  }

-- | Create a 'PlcResolver' pointing at the given PLC directory URL.
--
-- A fresh TLS-capable connection manager is created.
newPlcResolver :: T.Text -> IO PlcResolver
newPlcResolver url = do
  mgr <- newManager tlsManagerSettings
  return $ PlcResolver url mgr

-- | Create a 'PlcResolver' using the canonical AT Protocol PLC directory
-- (@https://plc.directory@).
defaultPlcResolver :: IO PlcResolver
defaultPlcResolver = newPlcResolver "https://plc.directory"

instance DidResolver PlcResolver where
  resolve r did = resolvePlc r did

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

resolvePlc :: PlcResolver -> T.Text -> IO (Either ResolveError DidDocument)
resolvePlc r did = do
  let url = T.unpack (plcDirectoryUrl r) ++ "/" ++ T.unpack did
  result <- try' (fetchJson (plcManager r) url)
  case result of
    Left  err -> return $ Left (DidNetworkError err)
    Right Nothing  -> return $ Left (DidNotFound did)
    Right (Just v) ->
      case Aeson.fromJSON v of
        Aeson.Error   msg -> return $ Left (DidParseError msg)
        Aeson.Success doc ->
          -- Validate that the document ID matches the requested DID.
          if didDocId doc == did
            then return $ Right doc
            else return $ Left $ DidParseError
                   "DID document 'id' field does not match requested DID"
  where
    didDocId = ATProto.DID.Document.didDocId

-- | Perform an HTTP GET and return the parsed JSON body.
--
-- Returns 'Nothing' on HTTP 404, 'Just' the JSON value on 2xx.
-- Throws on network errors or other HTTP failures.
fetchJson :: Manager -> String -> IO (Maybe Aeson.Value)
fetchJson mgr url = do
  req <- addAcceptJson <$> parseRequest url
  resp <- httpLbs req mgr
  let status = statusCode (responseStatus resp)
  case status of
    404 -> return Nothing
    _   | status >= 200 && status < 300 ->
            case Aeson.decode (responseBody resp) of
              Nothing -> fail ("Could not parse JSON from " ++ url)
              Just v  -> return (Just v)
        | otherwise ->
            fail ("HTTP " ++ show status ++ " from " ++ url)

-- | Add @Accept: application/json@ to a request.
addAcceptJson :: Request -> Request
addAcceptJson req =
  req { Network.HTTP.Client.requestHeaders =
          (hAccept, "application/json")
          : Network.HTTP.Client.requestHeaders req
      }
  where
    hAccept = TE.encodeUtf8 "Accept"

-- | Catch all synchronous exceptions and return them as 'Left String'.
try' :: IO a -> IO (Either String a)
try' action = catch (fmap Right action) handler
  where
    handler :: SomeException -> IO (Either String a)
    handler e = return $ Left (show e)
