-- | Core types for the ATProto OAuth 2.1 provider.
--
-- Error types, configuration, and access token payload for server-side
-- OAuth operations.
module ATProto.OAuth.Provider.Types
  ( -- * Configuration
    ProviderConfig (..)
    -- * Access token payload
  , AccessTokenPayload (..)
    -- * DPoP proof
  , DpopProof (..)
    -- * Errors
  , ProviderError (..)
  ) where

import qualified Data.Text as T
import           Data.Time (NominalDiffTime)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the OAuth provider.
--
-- The provider requires an issuer URL and timing parameters derived from
-- the upstream reference implementation.
data ProviderConfig = ProviderConfig
  { pcIssuer              :: T.Text
    -- ^ Canonical issuer URL (e.g. @\"https://bsky.social\"@).
    -- Must have no path, query, or fragment.
  , pcDpopNonceSecret     :: Maybe T.Text
    -- ^ Optional 32-byte hex-encoded secret for deterministic DPoP nonces.
    -- When 'Nothing', a random secret is generated at startup.
    -- When @Just \"false\"@, nonces are disabled entirely.
  , pcDpopNonceRotationMs :: Maybe Int
    -- ^ DPoP nonce rotation interval in milliseconds.
    -- Defaults to 60000 (60 seconds), matching the upstream maximum of
    -- @DPOP_NONCE_MAX_AGE / 3@.
  , pcAccessTokenLifetime :: NominalDiffTime
    -- ^ Access token lifetime.
    -- Upstream default is 3600 seconds (60 minutes).
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Access token payload
-- ---------------------------------------------------------------------------

-- | The payload of a verified access token.
--
-- Based on RFC 9068 (JWT Profile for Access Tokens).  The @cnf@ claim
-- contains the DPoP key thumbprint when the token is DPoP-bound.
data AccessTokenPayload = AccessTokenPayload
  { atpIss      :: T.Text
    -- ^ @iss@ – issuer URL.
  , atpSub      :: T.Text
    -- ^ @sub@ – subject DID.
  , atpAud      :: T.Text
    -- ^ @aud@ – audience (resource server URL).
  , atpJti      :: T.Text
    -- ^ @jti@ – unique token ID.
  , atpScope    :: T.Text
    -- ^ @scope@ – space-separated granted scopes.
  , atpClientId :: Maybe T.Text
    -- ^ @client_id@ – the client that requested the token.
  , atpIat      :: Int
    -- ^ @iat@ – issued-at (seconds since epoch).
  , atpExp      :: Int
    -- ^ @exp@ – expiry (seconds since epoch).
  , atpCnfJkt   :: Maybe T.Text
    -- ^ @cnf.jkt@ – JWK thumbprint of the DPoP key.
    -- Present when the token is DPoP-bound.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- DPoP proof
-- ---------------------------------------------------------------------------

-- | A verified DPoP proof, containing the extracted claims.
--
-- Returned by 'ATProto.OAuth.Provider.DPoP.Verifier.verifyDpopProof'
-- after successful verification.
data DpopProof = DpopProof
  { dpJti :: T.Text
    -- ^ Unique proof identifier (for replay detection).
  , dpJkt :: T.Text
    -- ^ JWK SHA-256 thumbprint of the proof's public key.
  , dpHtm :: T.Text
    -- ^ HTTP method from the proof.
  , dpHtu :: T.Text
    -- ^ HTTP URI from the proof.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors that can occur during OAuth provider operations.
data ProviderError
  = InvalidDpopProof T.Text
    -- ^ The DPoP proof is malformed or fails verification.
    -- The 'T.Text' is a human-readable description.
  | UseDpopNonce
    -- ^ The server requires a DPoP nonce but none was supplied,
    -- or the supplied nonce is stale.
  | InvalidToken T.Text
    -- ^ The access token is malformed, expired, or fails verification.
  | InvalidTokenBinding
    -- ^ The DPoP proof key does not match the token's @cnf.jkt@ claim.
  | MissingDpopProof
    -- ^ A DPoP-bound token was presented without a DPoP proof.
  deriving (Eq, Show)
