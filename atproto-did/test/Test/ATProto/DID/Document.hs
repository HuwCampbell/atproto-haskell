module Test.ATProto.DID.Document (tests) where

import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text             as T
import Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import ATProto.DID.Document

-- ---------------------------------------------------------------------------
-- Sample JSON fixtures
-- ---------------------------------------------------------------------------

-- | A realistic did:plc document (typical Bluesky account structure).
plcDocJson :: BL.ByteString
plcDocJson = BLC.pack $ unwords
  [ "{"
  , "\"@context\": [\"https://www.w3.org/ns/did/v1\"],"
  , "\"id\": \"did:plc:ewvi7nxzyoun6zhhandbv25b\","
  , "\"alsoKnownAs\": [\"at://haileyok.com\"],"
  , "\"verificationMethod\": [{"
  , "  \"id\": \"did:plc:ewvi7nxzyoun6zhhandbv25b#atproto\","
  , "  \"type\": \"EcdsaSecp256k1VerificationKey2019\","
  , "  \"controller\": \"did:plc:ewvi7nxzyoun6zhhandbv25b\","
  , "  \"publicKeyMultibase\": \"zQ3shhCGHariN4zeC7xAqYQRKAEaHeVtCHKRSMcBiRvYmM8o7\""
  , "}],"
  , "\"service\": [{"
  , "  \"id\": \"#atproto_pds\","
  , "  \"type\": \"AtprotoPersonalDataServer\","
  , "  \"serviceEndpoint\": \"https://morel.us-east.host.bsky.network\""
  , "}]"
  , "}"
  ]

-- | A did:web document using the Multikey verification type.
webDocJson :: BL.ByteString
webDocJson = BLC.pack $ unwords
  [ "{"
  , "\"id\": \"did:web:example.com\","
  , "\"verificationMethod\": [{"
  , "  \"id\": \"did:web:example.com#key-1\","
  , "  \"type\": \"Multikey\","
  , "  \"controller\": \"did:web:example.com\","
  , "  \"publicKeyMultibase\": \"zDnaerDaTF5BXEavCrfRZEk316dpbLsfPDZ3WJ5hine6q1NFE\""
  , "}],"
  , "\"service\": [{"
  , "  \"id\": \"#atproto_pds\","
  , "  \"type\": \"AtprotoPersonalDataServer\","
  , "  \"serviceEndpoint\": \"https://pds.example.com\""
  , "}]"
  , "}"
  ]

-- | A minimal document with no optional fields.
minimalDocJson :: BL.ByteString
minimalDocJson = BLC.pack "{\"id\": \"did:plc:minimal\"}"

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | The did:plc fixture parses to the expected structure.
prop_parsePlcDoc :: Property
prop_parsePlcDoc = withTests 1 . property $ do
  doc <- evalEither (Aeson.eitherDecode plcDocJson)
  didDocId doc                  === "did:plc:ewvi7nxzyoun6zhhandbv25b"
  didDocAlsoKnownAs doc         === ["at://haileyok.com"]
  length (didDocVerificationMethods doc) === 1
  length (didDocServices doc)   === 1
  let vm = head (didDocVerificationMethods doc)
  vmType vm                     === "EcdsaSecp256k1VerificationKey2019"
  vmPublicKeyMultibase vm       === Just "zQ3shhCGHariN4zeC7xAqYQRKAEaHeVtCHKRSMcBiRvYmM8o7"
  let svc = head (didDocServices doc)
  serviceType svc               === "AtprotoPersonalDataServer"
  serviceEndpoint svc           === "https://morel.us-east.host.bsky.network"

-- | The did:web fixture (Multikey type) parses correctly.
prop_parseWebDoc :: Property
prop_parseWebDoc = withTests 1 . property $ do
  doc <- evalEither (Aeson.eitherDecode webDocJson)
  didDocId doc                === "did:web:example.com"
  didDocAlsoKnownAs doc       === []
  length (didDocVerificationMethods doc) === 1
  let vm = head (didDocVerificationMethods doc)
  vmType vm                   === "Multikey"

-- | A document with only an 'id' field is valid (empty optional lists).
prop_parseMinimalDoc :: Property
prop_parseMinimalDoc = withTests 1 . property $ do
  doc <- evalEither (Aeson.eitherDecode minimalDocJson)
  didDocId doc                          === "did:plc:minimal"
  didDocAlsoKnownAs doc                 === []
  didDocVerificationMethods doc         === []
  didDocServices doc                    === []

-- | A document missing the required 'id' field fails to parse.
prop_missingIdFails :: Property
prop_missingIdFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"verificationMethod\": []}"
  case (Aeson.eitherDecode bad :: Either String DidDocument) of
    Left  _ -> success
    Right _ -> failure

-- | Extraneous fields (like @\@context@) are silently ignored.
prop_extraFieldsIgnored :: Property
prop_extraFieldsIgnored = withTests 1 . property $ do
  let json = BLC.pack "{\"id\":\"did:plc:x\",\"unknownField\":\"ignored\"}"
  doc <- evalEither (Aeson.eitherDecode json)
  didDocId doc === "did:plc:x"

-- | A verification method without 'publicKeyMultibase' has 'Nothing'.
prop_vmMissingKeyIsNothing :: Property
prop_vmMissingKeyIsNothing = withTests 1 . property $ do
  let json = BLC.pack $ unwords
        [ "{\"id\":\"did:plc:x\","
        , "\"verificationMethod\":[{"
        , "  \"id\":\"did:plc:x#k\","
        , "  \"type\":\"Multikey\","
        , "  \"controller\":\"did:plc:x\""
        , "}]}"
        ]
  doc <- evalEither (Aeson.eitherDecode json)
  let vm = head (didDocVerificationMethods doc)
  vmPublicKeyMultibase vm === Nothing

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "DID.Document"
  [ ("parse did:plc document",             prop_parsePlcDoc)
  , ("parse did:web document (Multikey)",  prop_parseWebDoc)
  , ("parse minimal document",             prop_parseMinimalDoc)
  , ("missing 'id' field fails",           prop_missingIdFails)
  , ("extraneous fields are ignored",      prop_extraFieldsIgnored)
  , ("missing publicKeyMultibase is Nothing", prop_vmMissingKeyIsNothing)
  ]
