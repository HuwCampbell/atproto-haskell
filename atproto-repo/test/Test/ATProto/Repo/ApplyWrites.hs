module Test.ATProto.Repo.ApplyWrites (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import ATProto.Lex.Json          (decode)
import ATProto.Repo.ApplyWrites

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleResponse :: BLC.ByteString
sampleResponse = BLC.pack $ unwords
  [ "{"
  , "  \"commit\": { \"cid\": \"bafyrei...\", \"rev\": \"3k7aaa\" },"
  , "  \"results\": ["
  , "    { \"$type\": \"com.atproto.repo.applyWrites#createResult\","
  , "      \"uri\": \"at://did:plc:abc/app.bsky.feed.post/3k7aa\","
  , "      \"cid\": \"bafyreicreated\" },"
  , "    { \"$type\": \"com.atproto.repo.applyWrites#deleteResult\" }"
  , "  ]"
  , "}"
  ]

emptyResponse :: BLC.ByteString
emptyResponse = BLC.pack "{}"

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_parseSampleResponse :: Property
prop_parseSampleResponse = withTests 1 . property $ do
  resp <- evalEither (decode applyWritesResponseCodec sampleResponse)
  case awresCommit resp of
    Just _ -> success
    Nothing -> failure
  case awresResults resp of
    Just results -> length results === 2
    Nothing -> failure

prop_parseEmptyResponse :: Property
prop_parseEmptyResponse = withTests 1 . property $ do
  resp <- evalEither (decode applyWritesResponseCodec emptyResponse)
  awresCommit resp  === Nothing
  awresResults resp === Nothing

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Repo.ApplyWrites"
  [ ("parse sample response",  prop_parseSampleResponse)
  , ("parse empty response",   prop_parseEmptyResponse)
  ]
