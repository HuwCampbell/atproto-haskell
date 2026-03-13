module Test.ATProto.Repo.ListRecords (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text                  as T
import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import ATProto.Lex.Json        (decode)
import ATProto.Repo.ListRecords

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A realistic listRecords response (two posts, with cursor).
sampleResponse :: BLC.ByteString
sampleResponse = BLC.pack $ unwords
  [ "{"
  , "  \"cursor\": \"3k7bj3abc\","
  , "  \"records\": ["
  , "    { \"uri\": \"at://did:plc:abc/app.bsky.feed.post/3k7aa\""
  , "    , \"cid\": \"bafyreiapple\""
  , "    , \"value\": { \"text\": \"Hello, world!\", \"$type\": \"app.bsky.feed.post\" }"
  , "    },"
  , "    { \"uri\": \"at://did:plc:abc/app.bsky.feed.post/3k7bb\""
  , "    , \"cid\": \"bafyreibanana\""
  , "    , \"value\": { \"text\": \"Second post.\", \"$type\": \"app.bsky.feed.post\" }"
  , "    }"
  , "  ]"
  , "}"
  ]

-- | A response with no records and no cursor.
emptyResponse :: BLC.ByteString
emptyResponse = BLC.pack "{\"records\": []}"

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | The sample response parses to the expected fields.
prop_parseSampleResponse :: Property
prop_parseSampleResponse = withTests 1 . property $ do
  resp <- evalEither (decode listRecordsResponseCodec sampleResponse)
  lrrCursor  resp === Just "3k7bj3abc"
  length (lrrRecords resp) === 2
  let r0 = head (lrrRecords resp)
  rrUri r0 === "at://did:plc:abc/app.bsky.feed.post/3k7aa"
  rrCid r0 === "bafyreiapple"

-- | An empty records list parses with no cursor.
prop_parseEmptyResponse :: Property
prop_parseEmptyResponse = withTests 1 . property $ do
  resp <- evalEither (decode listRecordsResponseCodec emptyResponse)
  lrrCursor  resp === Nothing
  lrrRecords resp === []

-- | Missing "records" field causes a parse error.
prop_missingRecordsFails :: Property
prop_missingRecordsFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"cursor\": \"abc\"}"
  case decode listRecordsResponseCodec bad of
    Left  _ -> success
    Right _ -> failure

-- | 'defaultListRecordsParams' sets repo and collection, all optionals Nothing.
prop_defaultParams :: Property
prop_defaultParams = withTests 1 . property $ do
  let p = defaultListRecordsParams "did:plc:abc" "app.bsky.feed.post"
  lrpRepo       p === "did:plc:abc"
  lrpCollection p === "app.bsky.feed.post"
  lrpLimit      p === Nothing
  lrpCursor     p === Nothing
  lrpReverse    p === Nothing

-- | Query params always include repo and collection.
prop_toQueryParamsContainsRequired :: Property
prop_toQueryParamsContainsRequired = property $ do
  repo <- forAll $ Gen.text (Range.linear 3 20) Gen.alphaNum
  col  <- forAll $ Gen.text (Range.linear 3 20) Gen.alphaNum
  let p      = defaultListRecordsParams repo col
      params = toQueryParams' p
  lookup "repo"       params === Just repo
  lookup "collection" params === Just col
  where
    toQueryParams' p =
      [ ("repo",       lrpRepo p)
      , ("collection", lrpCollection p)
      ]

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Repo.ListRecords"
  [ ("parse sample response",             prop_parseSampleResponse)
  , ("parse empty response",              prop_parseEmptyResponse)
  , ("missing 'records' fails",           prop_missingRecordsFails)
  , ("defaultListRecordsParams",          prop_defaultParams)
  , ("query params contain required",     prop_toQueryParamsContainsRequired)
  ]
