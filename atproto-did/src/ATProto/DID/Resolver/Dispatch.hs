-- | Resolver for @did@ identifiers.
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
-- @did:web@ resolves a DID by fetching a well-known JSON document from the
-- host encoded in the identifier.  For @did:web:example.com@ the URL is:
--
-- @
-- GET https://example.com/.well-known/did.json
-- Accept: application/json
-- @
--
-- This resolver uses an appropriate resolver depending on the did type.
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
module ATProto.DID.Resolver.Dispatch
  ( DispatchResolver (..)
  , newDispatchResolver
  , defaultDispatchResolver
  ) where

import qualified Data.Text                 as Text
import           ATProto.DID.Resolver
import           ATProto.DID.Resolver.PLC
import           ATProto.DID.Resolver.Web

-- | A resolver for known @did:plc@ identifiers.
data DispatchResolver = DispatchResolver
  { plcResolver :: PlcResolver
    -- ^ The PLC Resolver to dispatch to.
  , webResolver :: WebResolver
    -- ^ The Web Resolver to dispatch to.
  }

-- | Create a 'PlcResolver' pointing at the given PLC directory URL.
newDispatchResolver :: PlcResolver -> WebResolver -> DispatchResolver
newDispatchResolver = DispatchResolver

-- | Create a 'PlcResolver' using the canonical AT Protocol PLC directory.
defaultDispatchResolver :: IO DispatchResolver
defaultDispatchResolver = do
  DispatchResolver <$> newPlcResolver "https://plc.directory" <*> newWebResolver

instance DidResolver DispatchResolver where
  resolve (DispatchResolver plc web) did
    | "did:plc:" `Text.isPrefixOf` did = resolve plc did
    | "did:web:" `Text.isPrefixOf` did = resolve web did
    | otherwise = return (Left (DidUnsupported did))
