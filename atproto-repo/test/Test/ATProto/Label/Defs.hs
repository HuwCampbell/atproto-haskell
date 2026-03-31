module Test.ATProto.Label.Defs (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import ATProto.Lex.Json   (decode)
import ATProto.Label.Defs

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleLabel :: BLC.ByteString
sampleLabel = BLC.pack $ unwords
  [ "{"
  , "  \"ver\": 1,"
  , "  \"src\": \"did:plc:labeler\","
  , "  \"uri\": \"at://did:plc:abc/app.bsky.feed.post/3k7aa\","
  , "  \"cid\": \"bafyreirecord\","
  , "  \"val\": \"porn\","
  , "  \"neg\": false,"
  , "  \"cts\": \"2024-01-01T00:00:00Z\""
  , "}"
  ]

minimalLabel :: BLC.ByteString
minimalLabel = BLC.pack $ unwords
  [ "{"
  , "  \"src\": \"did:plc:labeler\","
  , "  \"uri\": \"at://did:plc:abc/app.bsky.feed.post/3k7aa\","
  , "  \"val\": \"!warn\","
  , "  \"cts\": \"2024-06-15T12:00:00Z\""
  , "}"
  ]

sampleSelfLabels :: BLC.ByteString
sampleSelfLabels = BLC.pack $ unwords
  [ "{"
  , "  \"values\": ["
  , "    { \"val\": \"nudity\" },"
  , "    { \"val\": \"sexual\" }"
  , "  ]"
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_parseSampleLabel :: Property
prop_parseSampleLabel = withTests 1 . property $ do
  lbl <- evalEither (decode labelCodec sampleLabel)
  labVer lbl === Just 1
  labSrc lbl === "did:plc:labeler"
  labUri lbl === "at://did:plc:abc/app.bsky.feed.post/3k7aa"
  labCid lbl === Just "bafyreirecord"
  labVal lbl === "porn"
  labNeg lbl === Just False
  labCts lbl === "2024-01-01T00:00:00Z"
  labExp lbl === Nothing
  labSig lbl === Nothing

prop_parseMinimalLabel :: Property
prop_parseMinimalLabel = withTests 1 . property $ do
  lbl <- evalEither (decode labelCodec minimalLabel)
  labVer lbl === Nothing
  labVal lbl === "!warn"
  labCid lbl === Nothing
  labNeg lbl === Nothing

prop_parseSelfLabels :: Property
prop_parseSelfLabels = withTests 1 . property $ do
  sl <- evalEither (decode selfLabelsCodec sampleSelfLabels)
  length (slValues sl) === 2
  case slValues sl of
    (v0 : v1 : _) -> do
      slVal v0 === "nudity"
      slVal v1 === "sexual"
    _ -> failure

prop_missingValFails :: Property
prop_missingValFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"src\": \"did:plc:x\", \"uri\": \"x\", \"cts\": \"2024-01-01T00:00:00Z\"}"
  case decode labelCodec bad of
    Left  _ -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Label.Defs"
  [ ("parse sample label",    prop_parseSampleLabel)
  , ("parse minimal label",   prop_parseMinimalLabel)
  , ("parse self-labels",     prop_parseSelfLabels)
  , ("missing 'val' fails",   prop_missingValFails)
  ]
