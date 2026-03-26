{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
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
module ATProto.DID.Resolver.Caching
  ( CachingResolver (..)
  , newCachingResolver
  , defaultCachingResolver
  ) where

import           Data.Foldable             (for_)
import           Data.Text                 as Text
import qualified Data.Cache                as Cache
import           System.Clock              (TimeSpec (..))

import           ATProto.DID.Document      (DidDocument )
import           ATProto.DID.Resolver      (DidResolver (..), CachingDidResolver(..))

-- | A resolver for known @did:plc@ identifiers.
data CachingResolver c =
  CachingResolver {
    wrappedResolver :: c
    -- ^ The PLC Resolver to dispatch to.
  , didCache :: Cache.Cache Text DidDocument
  }

-- | Create a 'PlcResolver' pointing at the given PLC directory URL.
newCachingResolver :: c -> TimeSpec -> IO (CachingResolver c)
newCachingResolver c x = do
    cache <- Cache.newCache (Just x)
    return $ CachingResolver c cache


-- | Create a caching resolver with an hour long cache time.
defaultCachingResolver :: c -> IO (CachingResolver c)
defaultCachingResolver wrapped = do
  newCachingResolver wrapped (TimeSpec 3600 0)


instance DidResolver c => DidResolver (CachingResolver c) where
  resolve (CachingResolver wrapped cache) did = do
    mDid <- Cache.lookup' cache did
    case mDid of
      Just x -> pure (Right x)
      Nothing -> do
        eFetched <- resolve wrapped did
        for_ eFetched $ \fetched ->
            Cache.insert cache did fetched

        return eFetched

instance DidResolver c => CachingDidResolver (CachingResolver c) where
  refreshResolve (CachingResolver wrapped cache) did = do
    eFetched <- resolve wrapped did
    for_ eFetched $ \fetched ->
      Cache.insert cache did fetched

    return eFetched
