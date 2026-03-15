-- | AT Protocol DID resolution.
--
-- The AT Protocol uses two DID methods:
--
-- * @did:plc@ – the primary method, backed by the PLC directory
--   (@https://plc.directory@).
-- * @did:web@ – resolved via @https://{host}/.well-known/did.json@.
--
-- A resolved DID document contains the account's handle(s) (@alsoKnownAs@),
-- signing key(s) (@verificationMethod@), and PDS URL (@service@).
--
-- = Typical usage
--
-- == Resolving a did:plc identifier
--
-- @
-- import ATProto.DID
--
-- main :: IO ()
-- main = do
--   -- Create a resolver pointed at the default PLC directory.
--   resolver <- defaultPlcResolver
--
--   result <- resolve resolver "did:plc:ewvi7nxzyoun6zhhandbv25b"
--   case result of
--     Left  (DidNotFound did) -> putStrLn ("Not found: " ++ show did)
--     Left  err               -> print err
--     Right doc               -> do
--       -- Handle, e.g. "at://haileyok.com"
--       print (didDocAlsoKnownAs doc)
--       -- PDS URL
--       mapM_ (print . serviceEndpoint) (didDocServices doc)
-- @
--
-- == Resolving a did:web identifier
--
-- @
--   webRes <- newWebResolver
--   result <- resolve webRes "did:web:example.com"
--   case result of
--     Left  err -> print err
--     Right doc -> print (didDocVerificationMethods doc)
-- @
--
-- == Dispatching over both methods
--
-- When the DID method is not known ahead of time, inspect the DID prefix
-- and choose the appropriate resolver:
--
-- @
--   import qualified Data.Text as T
--
--   resolveDid :: T.Text -> IO (Either ResolveError DidDocument)
--   resolveDid did
--     | "did:plc:" \`T.isPrefixOf\` did = do
--         r <- defaultPlcResolver
--         resolve r did
--     | "did:web:" \`T.isPrefixOf\` did = do
--         r <- newWebResolver
--         resolve r did
--     | otherwise =
--         return $ Left $ DidUnsupported did
-- @
module ATProto.DID
  ( -- * DID Document
    DidDocument (..)
  , VerificationMethod (..)
  , Service (..)
    -- * Resolver interface
  , DidResolver (..)
  , ResolveError (..)
    -- * did:plc resolver
  , PlcResolver
  , newPlcResolver
  , defaultPlcResolver
    -- * did:web resolver
  , WebResolver
  , newWebResolver
    -- * Dynamic did resolver
  , DispatchResolver
  , newDispatchResolver
  , defaultDispatchResolver
  ) where

import ATProto.DID.Document
import ATProto.DID.Resolver
import ATProto.DID.Resolver.Dispatch
import ATProto.DID.Resolver.PLC
import ATProto.DID.Resolver.Web
