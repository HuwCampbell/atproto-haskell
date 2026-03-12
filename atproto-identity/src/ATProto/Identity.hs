-- | AT Protocol handle-to-DID resolution.
--
-- The AT Protocol specification defines two mechanisms for resolving a
-- handle (e.g. @alice.bsky.social@) to a DID:
--
-- 1. A DNS TXT record on @_atproto.\<handle\>@ containing the value
--    @did=\<did\>@.
-- 2. An HTTP GET request to @https:\/\/\<handle\>\/.well-known\/atproto-did@
--    whose response body begins with @did:@.
--
-- The resolver tries DNS first.  If DNS returns nothing, it falls back to
-- HTTP.  This matches the behaviour of the reference TypeScript
-- implementation in @\@atproto\/identity@.
--
-- = Typical usage
--
-- @
-- import ATProto.Identity
--
-- main :: IO ()
-- main = do
--   resolver <- newHandleResolver defaultHandleResolverOpts
--   result   <- resolveHandle resolver "alice.bsky.social"
--   case result of
--     Nothing  -> putStrLn "Handle could not be resolved to a DID"
--     Just did -> putStrLn ("Resolved: " ++ show did)
-- @
module ATProto.Identity
  ( -- * Handle resolver
    HandleResolver
  , HandleResolverOpts (..)
  , defaultHandleResolverOpts
  , newHandleResolver
    -- * Resolution
  , resolveHandle
  , resolveHandleDns
  , resolveHandleHttp
  ) where

import ATProto.Identity.Handle
