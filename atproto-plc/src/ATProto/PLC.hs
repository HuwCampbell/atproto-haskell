-- | AT Protocol PLC directory client.
--
-- This module re-exports the full public API for creating and submitting
-- @did:plc@ operations and resolving DID documents from the PLC directory.
--
-- = Quick start
--
-- @
-- import ATProto.PLC
-- import ATProto.Crypto         (generateKeyPair, Curve(..))
-- import ATProto.Crypto.DidKey  (pubKeyToDidKey)
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Text       as T
--
-- main :: IO ()
-- main = do
--   -- Generate rotation and signing keys.
--   (rotPriv, rotPub)   <- generateKeyPair Secp256k1
--   (signPriv, signPub) <- generateKeyPair Secp256k1
--   let rotKeyDid  = T.pack (pubKeyToDidKey rotPub)
--       signKeyDid = T.pack (pubKeyToDidKey signPub)
--
--   -- Build the genesis operation.
--   let uop = UnsignedPlcOp
--               { uopRotationKeys        = [rotKeyDid]
--               , uopVerificationMethods = Map.singleton "atproto" signKeyDid
--               , uopAlsoKnownAs         = ["at://alice.example.com"]
--               , uopServices            = Map.singleton "atproto_pds"
--                   PlcService { plcServiceType     = "AtprotoPersonalDataServer"
--                              , plcServiceEndpoint = "https://pds.example.com" }
--               , uopPrev = Nothing
--               }
--
--   -- Derive the DID and sign the operation.
--   Right did <- return (plcOpDid uop)
--   op        <- signPlcOp rotPriv uop
--
--   -- Submit to the live PLC directory.
--   client <- newPlcClient defaultPlcEndpoint
--   result <- submitPlcOp client did op
--   case result of
--     Left  err -> putStrLn ("Error: " ++ show err)
--     Right ()  -> putStrLn ("DID created: " ++ show did)
-- @
module ATProto.PLC
  ( -- * Types
    module ATProto.PLC.Types
    -- * HTTP client
  , module ATProto.PLC.Client
  ) where

import ATProto.PLC.Types
import ATProto.PLC.Client
