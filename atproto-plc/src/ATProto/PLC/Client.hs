-- | HTTP client for the AT Protocol PLC directory.
--
-- The PLC directory is a registry for @did:plc@ identifiers.  This
-- module provides functions to:
--
-- * Submit signed PLC operations ('submitPlcOp') – creating or updating a DID.
-- * Resolve a @did:plc@ document ('resolvePlcDid').
--
-- By default the client targets the live PLC directory at
-- @https://plc.directory@, but a custom endpoint can be supplied for
-- testing or private deployments.
--
-- = Typical usage
--
-- @
-- import ATProto.PLC
-- import ATProto.Crypto (generateKeyPair, Curve(..))
-- import Data.Map.Strict qualified as Map
--
-- main :: IO ()
-- main = do
--   client              <- newPlcClient defaultPlcEndpoint
--   (rotPriv, rotPub)   <- generateKeyPair Secp256k1
--   (signPriv, signPub) <- generateKeyPair Secp256k1
--   let rotKey  = pubKeyToDidKey rotPub
--       signKey = pubKeyToDidKey signPub
--       uop = UnsignedPlcOp
--               { uopRotationKeys        = [T.pack rotKey]
--               , uopVerificationMethods = Map.singleton "atproto" (T.pack signKey)
--               , uopAlsoKnownAs         = ["at://alice.example.com"]
--               , uopServices            = Map.singleton "atproto_pds"
--                   PlcService{ plcServiceType = "AtprotoPersonalDataServer"
--                             , plcServiceEndpoint = "https://pds.example.com" }
--               , uopPrev = Nothing
--               }
--   Right did <- return (plcOpDid uop)
--   op   <- signPlcOp rotPriv uop
--   result <- submitPlcOp client did op
--   case result of
--     Left  err -> putStrLn ("Error: " ++ show err)
--     Right ()  -> putStrLn ("Created: " ++ show did)
-- @
module ATProto.PLC.Client
  ( -- * Client handle
    PlcClient
  , newPlcClient
  , newPlcClientWith
    -- * Endpoints
  , defaultPlcEndpoint
    -- * Operations
  , submitPlcOp
  , resolvePlcDid
    -- * Errors
  , PlcError (..)
  ) where

import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.Key         as Key
import qualified Data.Aeson.KeyMap      as KeyMap
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Vector            as Vec

import           Network.HTTP.Client     (Manager, Request, RequestBody (..),
                                          httpLbs, method, newManager,
                                          parseRequest, requestBody,
                                          requestHeaders, responseBody,
                                          responseStatus)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types      (statusCode)

import           ATProto.PLC.Types       (PlcOp (..), UnsignedPlcOp (..),
                                          PlcService (..))
import           ATProto.Syntax.DID      (DID, unDID)

-- ---------------------------------------------------------------------------
-- Client handle
-- ---------------------------------------------------------------------------

-- | A client for a PLC directory.
data PlcClient = PlcClient
  { plcClientEndpoint :: !T.Text
    -- ^ Base URL of the PLC directory (no trailing slash).
  , plcClientManager  :: !Manager
    -- ^ Shared TLS-capable connection manager.
  }

-- | The URL of the live PLC directory.
defaultPlcEndpoint :: T.Text
defaultPlcEndpoint = "https://plc.directory"

-- | Create a 'PlcClient' pointing at @endpoint@ with a fresh TLS manager.
newPlcClient :: T.Text -> IO PlcClient
newPlcClient endpoint = do
  mgr <- newManager tlsManagerSettings
  return (PlcClient endpoint mgr)

-- | Create a 'PlcClient' sharing an existing 'Manager'.
newPlcClientWith :: Manager -> T.Text -> PlcClient
newPlcClientWith mgr endpoint = PlcClient endpoint mgr

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors returned by PLC operations.
data PlcError
  = PlcHttpError Int T.Text
    -- ^ HTTP error: status code and body.
  | PlcDecodeError T.Text
    -- ^ Failed to decode the response body.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Submit a PLC operation
-- ---------------------------------------------------------------------------

-- | Submit a signed 'PlcOp' to the PLC directory.
--
-- Sends a @POST \<endpoint\>\/\<did\>@ with the operation serialised as
-- JSON.  Returns @Right ()@ on success (HTTP 2xx) or a 'PlcError' on
-- failure.
submitPlcOp :: PlcClient -> DID -> PlcOp -> IO (Either PlcError ())
submitPlcOp client did op = do
  let url  = T.unpack (plcClientEndpoint client) ++ "/" ++ T.unpack (unDID did)
      body = Aeson.encode (plcOpToJson op)
  req <- parseRequest url
  let req' = req
        { method         = "POST"
        , requestBody    = RequestBodyLBS body
        , requestHeaders = [("Content-Type", "application/json")]
        }
  resp <- httpLbs req' (plcClientManager client)
  let status = statusCode (responseStatus resp)
  if status >= 200 && status < 300
    then return (Right ())
    else return $ Left $ PlcHttpError status
           (TE.decodeUtf8 (BL.toStrict (responseBody resp)))

-- ---------------------------------------------------------------------------
-- Resolve a DID document
-- ---------------------------------------------------------------------------

-- | Fetch the DID document for a @did:plc@ identifier.
--
-- Sends a @GET \<endpoint\>\/\<did\>@ and returns the raw JSON object, or
-- a 'PlcError' on failure.
resolvePlcDid :: PlcClient -> DID -> IO (Either PlcError Aeson.Value)
resolvePlcDid client did = do
  let url = T.unpack (plcClientEndpoint client) ++ "/" ++ T.unpack (unDID did)
  req  <- parseRequest url
  resp <- httpLbs req (plcClientManager client)
  let status = statusCode (responseStatus resp)
      body   = responseBody resp
  if status >= 200 && status < 300
    then case Aeson.decode body of
           Nothing -> return (Left (PlcDecodeError "Failed to decode DID document"))
           Just v  -> return (Right v)
    else return $ Left $ PlcHttpError status
           (TE.decodeUtf8 (BL.toStrict body))

-- ---------------------------------------------------------------------------
-- JSON encoding for PLC operations
-- ---------------------------------------------------------------------------

-- | Serialise a 'PlcOp' as a JSON 'Aeson.Value'.
plcOpToJson :: PlcOp -> Aeson.Value
plcOpToJson op =
  let uop = popUnsigned op
  in  Aeson.Object $ KeyMap.fromList
        [ (Key.fromText "type",                Aeson.String "plc_operation")
        , (Key.fromText "rotationKeys",        rotKeysJson (uopRotationKeys uop))
        , (Key.fromText "verificationMethods", verifMethodsJson (uopVerificationMethods uop))
        , (Key.fromText "alsoKnownAs",         alsoKnownAsJson (uopAlsoKnownAs uop))
        , (Key.fromText "services",            servicesJson (uopServices uop))
        , (Key.fromText "prev",                prevJson (uopPrev uop))
        , (Key.fromText "sig",                 Aeson.String (popSig op))
        ]

rotKeysJson :: [T.Text] -> Aeson.Value
rotKeysJson = Aeson.Array . Vec.fromList . map Aeson.String

verifMethodsJson :: Map.Map T.Text T.Text -> Aeson.Value
verifMethodsJson m = Aeson.Object
  (KeyMap.fromList [ (Key.fromText k, Aeson.String v) | (k, v) <- Map.toList m ])

alsoKnownAsJson :: [T.Text] -> Aeson.Value
alsoKnownAsJson = Aeson.Array . Vec.fromList . map Aeson.String

prevJson :: Maybe T.Text -> Aeson.Value
prevJson Nothing    = Aeson.Null
prevJson (Just cid) = Aeson.String cid

servicesJson :: Map.Map T.Text PlcService -> Aeson.Value
servicesJson m = Aeson.Object $ KeyMap.fromList
  [ ( Key.fromText k
    , Aeson.Object $ KeyMap.fromList
        [ (Key.fromText "type",     Aeson.String (plcServiceType svc))
        , (Key.fromText "endpoint", Aeson.String (plcServiceEndpoint svc))
        ]
    )
  | (k, svc) <- Map.toList m
  ]
