-- | AT Protocol Lexicon schema parsing.
--
-- A Lexicon document is a JSON file that describes the schema of an AT
-- Protocol XRPC method or record type.  This module exposes the Haskell
-- types that represent a parsed Lexicon document together with the
-- 'Data.Aeson.FromJSON' instances needed to decode them from JSON.
--
-- = Typical usage
--
-- == Parsing a @.lexicon.json@ file
--
-- @
-- import ATProto.Lexicon
-- import qualified Data.Aeson           as Aeson
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Map.Strict      as Map
--
-- main :: IO ()
-- main = do
--   raw <- BL.readFile "lexicons/com/atproto/repo/listRecords.json"
--   case Aeson.eitherDecode raw of
--     Left  err -> putStrLn ("Parse error: " ++ err)
--     Right doc -> do
--       putStrLn ("Lexicon id: " ++ show (lexDocId doc))
--       case Map.lookup "main" (lexDocDefs doc) of
--         Just (LexQuery q) ->
--           print (lexXrpcQueryParameters q)
--         _ -> putStrLn "No query definition found"
-- @
--
-- == Inspecting the @com.atproto.repo.listRecords@ schema
--
-- @
--   -- The "main" definition is always the primary entry-point.
--   -- For a query, parameters describe the allowed query-string fields.
--   let Just (LexQuery q) = Map.lookup "main" (lexDocDefs doc)
--       Just params        = lexXrpcQueryParameters q
--       props              = lexXrpcParamsProperties params
--   mapM_ print (Map.keys props)
--   -- "collection", "cursor", "limit", "repo", "reverse"
-- @
module ATProto.Lexicon
  ( -- * Lexicon document
    LexiconDoc (..)
    -- * Top-level user types
  , LexUserType (..)
    -- * XRPC schemas
  , LexXrpcQuery (..)
  , LexXrpcProcedure (..)
  , LexXrpcSubscription (..)
  , LexXrpcParameters (..)
  , LexXrpcBody (..)
  , LexXrpcError (..)
    -- * Record schema
  , LexRecord (..)
    -- * Object and array types
  , LexObject (..)
  , LexObjectProperty (..)
  , LexArray (..)
  , LexToken (..)
    -- * Primitive types
  , LexPrimitive (..)
  , LexBoolean (..)
  , LexInteger (..)
  , LexString (..)
  , LexStringFormat (..)
  , LexUnknown (..)
    -- * IPLD types
  , LexIpldType (..)
  , LexBytes (..)
  , LexCidLink (..)
    -- * References
  , LexRefVariant (..)
  , LexRef (..)
  , LexRefUnion (..)
    -- * Blob
  , LexBlob (..)
  ) where

import ATProto.Lexicon.Types
-- Import Json to bring FromJSON instances into scope for users of this module.
import ATProto.Lexicon.Json ()
