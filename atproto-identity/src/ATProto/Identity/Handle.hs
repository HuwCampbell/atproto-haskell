-- | AT Protocol handle-to-DID resolution.
--
-- Implements the two resolution methods defined in the AT Protocol
-- specification (<https://atproto.com/specs/handle#handle-resolution>):
--
-- * DNS TXT record: @_atproto.\<handle\>@ containing @did=\<did\>@.
-- * HTTP well-known: @GET https:\/\/\<handle\>\/.well-known\/atproto-did@
--   with a response body that starts with @did:@.
--
-- This is a close port of the @HandleResolver@ class in
-- @\@atproto\/identity\/src\/handle\/index.ts@.
module ATProto.Identity.Handle
  ( -- * Resolver type and configuration
    HandleResolver
  , HandleResolverOpts (..)
  , defaultHandleResolverOpts
  , newHandleResolver
    -- * Resolution
  , resolveHandle
  , resolveHandleDns
  , resolveHandleHttp
    -- * Exported for testing
  , parseDnsResult
  , dnsAtprotoPrefix
  , handleToWellKnownUrl
  ) where

import           Control.Exception         (SomeException, catch)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Network.DNS               (FileOrNumericHost (..),
                                            ResolvConf (..), ResolvSeed,
                                            defaultResolvConf,
                                            makeResolvSeed, withResolver)
import qualified Network.DNS               as DNS
import           Network.HTTP.Client       (Manager, httpLbs, newManager,
                                            parseRequest, responseBody,
                                            responseStatus)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Configuration options for 'HandleResolver'.
data HandleResolverOpts = HandleResolverOpts
  { hroBackupNameserverIps :: [String]
    -- ^ Numeric IP addresses of backup DNS nameservers to try if the
    -- system resolver returns no result.  Host names are not accepted;
    -- use IP addresses only (e.g. @\"8.8.8.8\"@).
    --
    -- Mirrors the @backupNameservers@ field in @\@atproto\/identity@.
    -- The TypeScript implementation resolves hostnames to IPs at runtime;
    -- we require IPs directly to avoid the extra resolution step.
  }

-- | Default options: no backup nameservers.
defaultHandleResolverOpts :: HandleResolverOpts
defaultHandleResolverOpts = HandleResolverOpts
  { hroBackupNameserverIps = []
  }

-- | A resolver that maps AT Protocol handles to DID strings.
data HandleResolver = HandleResolver
  { hrResolvSeed :: ResolvSeed
    -- ^ Seed built from the system @resolv.conf@.
  , hrManager    :: Manager
    -- ^ Shared TLS-capable HTTP connection manager.
  , hrOpts       :: HandleResolverOpts
  }

-- | Construct a 'HandleResolver' using the given options and the system DNS
-- resolver configuration.
newHandleResolver :: HandleResolverOpts -> IO HandleResolver
newHandleResolver opts = do
  seed <- makeResolvSeed defaultResolvConf
  mgr  <- newManager tlsManagerSettings
  return (HandleResolver seed mgr opts)

-- ---------------------------------------------------------------------------
-- Resolution
-- ---------------------------------------------------------------------------

-- | Resolve an AT Protocol handle to its DID.
--
-- Strategy (matches the TypeScript reference implementation):
--
-- 1. Try DNS TXT lookup on @_atproto.\<handle\>@.
-- 2. If DNS yields nothing, try HTTP GET on the well-known URL.
-- 3. If both fail and backup nameserver IPs are configured, retry DNS
--    through those servers.
--
-- Returns 'Nothing' when all methods are exhausted without finding a DID.
resolveHandle :: HandleResolver -> T.Text -> IO (Maybe T.Text)
resolveHandle r handle = do
  dnsResult <- resolveHandleDns r handle
  case dnsResult of
    Just did -> return (Just did)
    Nothing  -> do
      httpResult <- resolveHandleHttp r handle
      case httpResult of
        Just did -> return (Just did)
        Nothing  -> resolveHandleDnsBackup r handle

-- | Resolve a handle via DNS TXT record lookup.
--
-- Queries TXT records for @_atproto.\<handle\>@ and returns the DID from
-- the first record that matches @did=\<did\>@.  Returns 'Nothing' on any
-- DNS error or when no matching record exists.
resolveHandleDns :: HandleResolver -> T.Text -> IO (Maybe T.Text)
resolveHandleDns r handle =
  withResolver (hrResolvSeed r) $ \resolver -> do
    let qname = BC.pack ("_atproto." ++ T.unpack handle)
    result <- DNS.lookupTXT resolver qname
    case result of
      Left  _    -> return Nothing
      Right txts -> return (parseDnsResult txts)

-- | Resolve a handle via the HTTP well-known endpoint.
--
-- Issues @GET https:\/\/\<handle\>\/.well-known\/atproto-did@.  Trims the
-- first line of the response body; if it begins with @did:@ it is returned
-- as the DID.  Any network or HTTP error silently returns 'Nothing'.
resolveHandleHttp :: HandleResolver -> T.Text -> IO (Maybe T.Text)
resolveHandleHttp r handle =
  catch go (\e -> let _ = e :: SomeException in return Nothing)
  where
    go :: IO (Maybe T.Text)
    go = do
      let url = handleToWellKnownUrl handle
      req  <- parseRequest url
      resp <- httpLbs req (hrManager r)
      let status = statusCode (responseStatus resp)
      if status == 200
        then
          let body      = TE.decodeUtf8Lenient (BL.toStrict (responseBody resp))
              firstLine = T.strip (head (T.lines body ++ [""]))
          in  if "did:" `T.isPrefixOf` firstLine
                then return (Just firstLine)
                else return Nothing
        else return Nothing

-- | Retry DNS resolution using backup nameserver IPs.
resolveHandleDnsBackup :: HandleResolver -> T.Text -> IO (Maybe T.Text)
resolveHandleDnsBackup r handle =
  case hroBackupNameserverIps (hrOpts r) of
    [] -> return Nothing
    ips -> do
      let conf = defaultResolvConf { resolvInfo = RCHostNames ips }
      seed <- makeResolvSeed conf
      withResolver seed $ \resolver -> do
        let qname = BC.pack ("_atproto." ++ T.unpack handle)
        result <- DNS.lookupTXT resolver qname
        case result of
          Left  _    -> return Nothing
          Right txts -> return (parseDnsResult txts)

-- ---------------------------------------------------------------------------
-- Pure helpers (exported for testing)
-- ---------------------------------------------------------------------------

-- | Parse a list of DNS TXT records into a DID.
--
-- In @dns-4.x@, @lookupTXT@ returns each TXT record as a single
-- already-joined 'BC.ByteString'.  Exactly one record must match
-- @did=\<did\>@; if zero or more than one match, 'Nothing' is returned.
--
-- This is a direct port of @parseDnsResult@ in the TypeScript implementation.
parseDnsResult :: [BC.ByteString] -> Maybe T.Text
parseDnsResult results =
  let decoded = map TE.decodeUtf8Lenient results
      found   = filter (T.isPrefixOf dnsAtprotoPrefix) decoded
  in  case found of
        [one] -> Just (T.drop (T.length dnsAtprotoPrefix) one)
        _     -> Nothing

-- | The DNS TXT record prefix that signals an AT Protocol DID.
dnsAtprotoPrefix :: T.Text
dnsAtprotoPrefix = "did="

-- | Build the HTTP well-known URL for a given handle.
handleToWellKnownUrl :: T.Text -> String
handleToWellKnownUrl handle =
  "https://" ++ T.unpack handle ++ "/.well-known/atproto-did"
