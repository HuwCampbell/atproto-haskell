-- | DID document data types and JSON parsing.
--
-- A resolved DID document contains three key pieces of information used by
-- the AT Protocol:
--
-- * @alsoKnownAs@ – the handle(s) associated with the DID
-- * @verificationMethod@ – the public signing key(s)
-- * @service@ – the PDS endpoint URL
--
-- Both the @did:plc@ and @did:web@ resolvers return JSON bodies that are
-- parsed into 'DidDocument' values using the 'Data.Aeson.FromJSON' instance
-- defined here.
module ATProto.DID.Document
  ( -- * DID Document
    DidDocument (..)
    -- * Verification methods
  , VerificationMethod (..)
    -- * Services
  , Service (..)
  ) where

import qualified Data.Aeson       as Aeson
import qualified Data.Text        as T

-- | A resolved DID document.
--
-- This is a simplified representation covering the fields used by the
-- AT Protocol; the full W3C DID Core model allows additional extension
-- properties which are ignored during parsing.
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
  { vmId                 :: T.Text
    -- ^ Fragment-qualified identifier, e.g. @\"did:plc:abc#atproto\"@.
  , vmType               :: T.Text
    -- ^ Key type, e.g. @\"Multikey\"@ or
    -- @\"EcdsaSecp256k1VerificationKey2019\"@.
  , vmController         :: T.Text
    -- ^ The DID that controls this key.
  , vmPublicKeyMultibase :: Maybe T.Text
    -- ^ The base-58btc-encoded multibase public key (present for
    -- @\"Multikey\"@ and legacy @\"EcdsaSecp256*\"@ types).
  } deriving (Eq, Show)

-- | A service endpoint entry in a DID document.
data Service = Service
  { serviceId       :: T.Text
    -- ^ Fragment-qualified identifier, e.g. @\"#atproto_pds\"@.
  , serviceType     :: T.Text
    -- ^ Service type, e.g. @\"AtprotoPersonalDataServer\"@.
  , serviceEndpoint :: T.Text
    -- ^ URL of the service endpoint.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- JSON parsing
-- ---------------------------------------------------------------------------

instance Aeson.FromJSON DidDocument where
  parseJSON = Aeson.withObject "DidDocument" $ \o ->
    DidDocument
      <$> o Aeson..:  "id"
      <*> o Aeson..:? "alsoKnownAs"       Aeson..!= []
      <*> o Aeson..:? "verificationMethod" Aeson..!= []
      <*> o Aeson..:? "service"            Aeson..!= []

instance Aeson.FromJSON VerificationMethod where
  parseJSON = Aeson.withObject "VerificationMethod" $ \o ->
    VerificationMethod
      <$> o Aeson..:  "id"
      <*> o Aeson..:  "type"
      <*> o Aeson..:  "controller"
      <*> o Aeson..:? "publicKeyMultibase"

instance Aeson.FromJSON Service where
  parseJSON = Aeson.withObject "Service" $ \o ->
    Service
      <$> o Aeson..: "id"
      <*> o Aeson..: "type"
      <*> o Aeson..: "serviceEndpoint"
