module Test.ATProto.Server.DescribeServer (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import ATProto.Lex.Json              (decode)
import ATProto.Server.DescribeServer

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleResponse :: BLC.ByteString
sampleResponse = BLC.pack $ unwords
  [ "{"
  , "  \"did\": \"did:web:example.com\","
  , "  \"availableUserDomains\": [\"example.com\", \"bsky.social\"],"
  , "  \"inviteCodeRequired\": true,"
  , "  \"phoneVerificationRequired\": false,"
  , "  \"links\": {"
  , "    \"privacyPolicy\": \"https://example.com/privacy\","
  , "    \"termsOfService\": \"https://example.com/tos\""
  , "  },"
  , "  \"contact\": {"
  , "    \"email\": \"admin@example.com\""
  , "  }"
  , "}"
  ]

minimalResponse :: BLC.ByteString
minimalResponse = BLC.pack $ unwords
  [ "{"
  , "  \"did\": \"did:web:example.com\","
  , "  \"availableUserDomains\": []"
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_parseSampleResponse :: Property
prop_parseSampleResponse = withTests 1 . property $ do
  resp <- evalEither (decode describeServerResponseCodec sampleResponse)
  dsrDid resp                  === "did:web:example.com"
  dsrAvailableUserDomains resp === ["example.com", "bsky.social"]
  dsrInviteCodeRequired resp   === Just True
  dsrPhoneVerificationRequired resp === Just False
  case dsrLinks resp of
    Just links -> do
      dslPrivacyPolicy links  === Just "https://example.com/privacy"
      dslTermsOfService links === Just "https://example.com/tos"
    Nothing -> failure
  case dsrContact resp of
    Just contact -> dscEmail contact === Just "admin@example.com"
    Nothing -> failure

prop_parseMinimalResponse :: Property
prop_parseMinimalResponse = withTests 1 . property $ do
  resp <- evalEither (decode describeServerResponseCodec minimalResponse)
  dsrDid resp                  === "did:web:example.com"
  dsrAvailableUserDomains resp === []
  dsrInviteCodeRequired resp   === Nothing
  dsrLinks resp                === Nothing
  dsrContact resp              === Nothing

prop_missingDidFails :: Property
prop_missingDidFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"availableUserDomains\": []}"
  case decode describeServerResponseCodec bad of
    Left  _ -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Server.DescribeServer"
  [ ("parse sample response",   prop_parseSampleResponse)
  , ("parse minimal response",  prop_parseMinimalResponse)
  , ("missing 'did' fails",     prop_missingDidFails)
  ]
