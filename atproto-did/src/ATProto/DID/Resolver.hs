-- | DID resolution interface.
--
-- 'DidResolver' is a typeclass that abstracts over the two DID methods
-- used by the AT Protocol:
--
-- * 'ATProto.DID.Resolver.PLC' – resolves @did:plc:…@ identifiers against
--   a PLC directory (default: @https://plc.directory@).
-- * 'ATProto.DID.Resolver.Web' – resolves @did:web:…@ identifiers by
--   fetching @https://{host}/.well-known/did.json@.
module ATProto.DID.Resolver
  ( -- * Typeclass
    DidResolver (..)
    -- * Errors
  , ResolveError (..)
  ) where

import qualified Data.Text        as T

import ATProto.DID.Document (DidDocument)

-- | Errors that can occur during DID resolution.
data ResolveError
  = DidNotFound T.Text
    -- ^ The DID does not exist (HTTP 404 or equivalent).
  | DidNetworkError String
    -- ^ A network or HTTP-level failure occurred.
  | DidParseError String
    -- ^ The server returned a response that could not be parsed as a
    -- valid DID document.
  | DidUnsupported T.Text
    -- ^ The DID method is not supported by this resolver.
  deriving (Eq, Show)

-- | A typeclass for types that can resolve a DID string to a 'DidDocument'.
--
-- Minimal complete definition: 'resolve'.
class DidResolver r where
  -- | Attempt to resolve a DID.
  --
  -- Returns 'Right' with the 'DidDocument' on success, or 'Left' with a
  -- 'ResolveError' describing the failure.  Network-level IO exceptions
  -- (e.g. connection refused) are propagated as-is.
  resolve :: r -> T.Text -> IO (Either ResolveError DidDocument)
