module Test.ATProto.Identity.ResolveHandle (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import ATProto.Lex.Json                (decode)
import ATProto.Identity.ResolveHandle

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleResponse :: BLC.ByteString
sampleResponse = BLC.pack "{\"did\": \"did:plc:abc123\"}"

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_parseSampleResponse :: Property
prop_parseSampleResponse = withTests 1 . property $ do
  resp <- evalEither (decode resolveHandleResponseCodec sampleResponse)
  rhrDid resp === "did:plc:abc123"

prop_missingDidFails :: Property
prop_missingDidFails = withTests 1 . property $ do
  let bad = BLC.pack "{}"
  case decode resolveHandleResponseCodec bad of
    Left  _ -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Identity.ResolveHandle"
  [ ("parse sample response",  prop_parseSampleResponse)
  , ("missing 'did' fails",    prop_missingDidFails)
  ]
