module Test.ATProto.Server.CreateSession (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import ATProto.Lex.Json             (decode)
import ATProto.Server.CreateSession

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleResponse :: BLC.ByteString
sampleResponse = BLC.pack $ unwords
  [ "{"
  , "  \"accessJwt\": \"eyJ..access\","
  , "  \"refreshJwt\": \"eyJ..refresh\","
  , "  \"handle\": \"alice.example.com\","
  , "  \"did\": \"did:plc:abc123\","
  , "  \"email\": \"alice@example.com\","
  , "  \"emailConfirmed\": true,"
  , "  \"active\": true"
  , "}"
  ]

minimalResponse :: BLC.ByteString
minimalResponse = BLC.pack $ unwords
  [ "{"
  , "  \"accessJwt\": \"tok\","
  , "  \"refreshJwt\": \"ref\","
  , "  \"handle\": \"bob.test\","
  , "  \"did\": \"did:plc:xyz\""
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_parseSampleResponse :: Property
prop_parseSampleResponse = withTests 1 . property $ do
  resp <- evalEither (decode createSessionResponseCodec sampleResponse)
  csresAccessJwt resp      === "eyJ..access"
  csresRefreshJwt resp     === "eyJ..refresh"
  csresHandle resp         === "alice.example.com"
  csresDid resp            === "did:plc:abc123"
  csresEmail resp          === Just "alice@example.com"
  csresEmailConfirmed resp === Just True
  csresActive resp         === Just True
  csresStatus resp         === Nothing

prop_parseMinimalResponse :: Property
prop_parseMinimalResponse = withTests 1 . property $ do
  resp <- evalEither (decode createSessionResponseCodec minimalResponse)
  csresAccessJwt resp      === "tok"
  csresDid resp            === "did:plc:xyz"
  csresEmail resp          === Nothing
  csresEmailConfirmed resp === Nothing

prop_missingFieldFails :: Property
prop_missingFieldFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"accessJwt\": \"tok\", \"handle\": \"h\", \"did\": \"did:plc:x\"}"
  case decode createSessionResponseCodec bad of
    Left  _ -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Server.CreateSession"
  [ ("parse sample response",   prop_parseSampleResponse)
  , ("parse minimal response",  prop_parseMinimalResponse)
  , ("missing 'refreshJwt' fails", prop_missingFieldFails)
  ]
