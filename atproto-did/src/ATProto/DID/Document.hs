-- | DID (Decentralised Identifier) document types.
--
-- Port of \@atproto\/did – the data types representing a resolved DID
-- document as defined by <https://www.w3.org/TR/did-core/>.
module ATProto.DID.Document
  ( -- * DID Document
    DidDocument (..)
    -- * Verification methods
  , VerificationMethod (..)
    -- * Services
  , Service (..)
  ) where

import qualified Data.Text as T

-- | A resolved DID document.
--
-- This is a simplified representation covering the fields used by the
-- AT Protocol; the full W3C DID Core model allows additional extension
-- properties.
data DidDocument = DidDocument
  { didDocId                  :: T.Text
    -- ^ The DID string itself, e.g. @\"did:plc:abc\"@.
  , didDocAlsoKnownAs         :: [T.Text]
    -- ^ Alternative identifiers (AT Protocol uses this for handles).
  , didDocVerificationMethods :: [VerificationMethod]
    -- ^ Public key material attached to this DID.
  , didDocServices            :: [Service]
    -- ^ Service endpoints (e.g. PDS URL).
  } deriving (Eq, Show)

-- | A public-key verification method entry in a DID document.
data VerificationMethod = VerificationMethod
  { vmId                  :: T.Text
    -- ^ Fragment-qualified identifier, e.g. @\"did:plc:abc#atproto\"@.
  , vmType                :: T.Text
    -- ^ Key type, e.g. @\"Multikey\"@ or @\"EcdsaSecp256k1VerificationKey2019\"@.
  , vmController          :: T.Text
    -- ^ The DID that controls this key.
  , vmPublicKeyMultibase  :: Maybe T.Text
    -- ^ The base-58-btc-encoded multibase public key (for 'Multikey').
  } deriving (Eq, Show)

-- | A service endpoint entry in a DID document.
data Service = Service
  { serviceId              :: T.Text
    -- ^ Fragment-qualified identifier, e.g. @\"did:plc:abc#atproto_pds\"@.
  , serviceType            :: T.Text
    -- ^ Service type, e.g. @\"AtprotoPersonalDataServer\"@.
  , serviceEndpoint        :: T.Text
    -- ^ URL of the service endpoint.
  } deriving (Eq, Show)
