module Test.ATProto.Sync.ListRepos (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import ATProto.Lex.Json       (decode)
import ATProto.Sync.ListRepos

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleResponse :: BLC.ByteString
sampleResponse = BLC.pack $ unwords
  [ "{"
  , "  \"cursor\": \"cursor123\","
  , "  \"repos\": ["
  , "    {"
  , "      \"did\": \"did:plc:abc\","
  , "      \"head\": \"bafyreicommit1\","
  , "      \"rev\": \"3k7aaa\","
  , "      \"active\": true"
  , "    },"
  , "    {"
  , "      \"did\": \"did:plc:xyz\","
  , "      \"head\": \"bafyreicommit2\","
  , "      \"rev\": \"3k7bbb\","
  , "      \"active\": false,"
  , "      \"status\": \"deactivated\""
  , "    }"
  , "  ]"
  , "}"
  ]

emptyResponse :: BLC.ByteString
emptyResponse = BLC.pack "{\"repos\": []}"

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_parseSampleResponse :: Property
prop_parseSampleResponse = withTests 1 . property $ do
  resp <- evalEither (decode listReposResponseCodec sampleResponse)
  lrsrCursor resp === Just "cursor123"
  length (lrsrRepos resp) === 2
  case lrsrRepos resp of
    (r0 : r1 : _) -> do
      lrrDid r0    === "did:plc:abc"
      lrrHead r0   === "bafyreicommit1"
      lrrRev r0    === "3k7aaa"
      lrrActive r0 === Just True
      lrrStatus r0 === Nothing
      lrrDid r1    === "did:plc:xyz"
      lrrActive r1 === Just False
      lrrStatus r1 === Just "deactivated"
    _ -> failure

prop_parseEmptyResponse :: Property
prop_parseEmptyResponse = withTests 1 . property $ do
  resp <- evalEither (decode listReposResponseCodec emptyResponse)
  lrsrCursor resp === Nothing
  lrsrRepos resp  === []

prop_missingReposFails :: Property
prop_missingReposFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"cursor\": \"abc\"}"
  case decode listReposResponseCodec bad of
    Left  _ -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Sync.ListRepos"
  [ ("parse sample response", prop_parseSampleResponse)
  , ("parse empty response",  prop_parseEmptyResponse)
  , ("missing 'repos' fails", prop_missingReposFails)
  ]
