module Test.ATProto.Lexicon.Json (tests) where

import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import           Hedgehog

import ATProto.Lexicon

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | The real com.atproto.repo.listRecords lexicon JSON.
listRecordsJson :: BLC.ByteString
listRecordsJson = BLC.pack $ unlines
  [ "{"
  , "  \"lexicon\": 1,"
  , "  \"id\": \"com.atproto.repo.listRecords\","
  , "  \"defs\": {"
  , "    \"main\": {"
  , "      \"type\": \"query\","
  , "      \"description\": \"List a range of records in a repository.\","
  , "      \"parameters\": {"
  , "        \"type\": \"params\","
  , "        \"required\": [\"repo\", \"collection\"],"
  , "        \"properties\": {"
  , "          \"repo\":       { \"type\": \"string\", \"format\": \"at-identifier\" },"
  , "          \"collection\": { \"type\": \"string\", \"format\": \"nsid\" },"
  , "          \"limit\":      { \"type\": \"integer\", \"minimum\": 1, \"maximum\": 100, \"default\": 50 },"
  , "          \"cursor\":     { \"type\": \"string\" },"
  , "          \"reverse\":    { \"type\": \"boolean\" }"
  , "        }"
  , "      },"
  , "      \"output\": {"
  , "        \"encoding\": \"application/json\","
  , "        \"schema\": {"
  , "          \"type\": \"object\","
  , "          \"required\": [\"records\"],"
  , "          \"properties\": {"
  , "            \"cursor\":  { \"type\": \"string\" },"
  , "            \"records\": {"
  , "              \"type\": \"array\","
  , "              \"items\": { \"type\": \"ref\", \"ref\": \"#record\" }"
  , "            }"
  , "          }"
  , "        }"
  , "      }"
  , "    },"
  , "    \"record\": {"
  , "      \"type\": \"object\","
  , "      \"required\": [\"uri\", \"cid\", \"value\"],"
  , "      \"properties\": {"
  , "        \"uri\":   { \"type\": \"string\", \"format\": \"at-uri\" },"
  , "        \"cid\":   { \"type\": \"string\", \"format\": \"cid\" },"
  , "        \"value\": { \"type\": \"unknown\" }"
  , "      }"
  , "    }"
  , "  }"
  , "}"
  ]

-- | A minimal valid lexicon (just an id and a token def).
minimalJson :: BLC.ByteString
minimalJson = BLC.pack $ unlines
  [ "{ \"lexicon\": 1"
  , ", \"id\": \"io.example.ping\""
  , ", \"defs\": {"
  , "    \"main\": { \"type\": \"token\", \"description\": \"A ping token.\" }"
  , "  }"
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | com.atproto.repo.listRecords parses to the expected structure.
prop_parseListRecords :: Property
prop_parseListRecords = withTests 1 . property $ do
  doc <- evalEither (Aeson.eitherDecode listRecordsJson)

  -- Top-level fields
  lexDocLexicon doc === 1
  lexDocId doc      === "com.atproto.repo.listRecords"

  -- Should have exactly two defs: "main" and "record"
  Map.size (lexDocDefs doc) === 2

  -- "main" must be a query
  case Map.lookup "main" (lexDocDefs doc) of
    Just (LexQuery q) -> do
      -- Has parameters
      case lexXrpcQueryParameters q of
        Nothing -> failure
        Just params -> do
          let props = lexXrpcParamsProperties params
          -- All five parameter names present
          Map.member "repo"       props === True
          Map.member "collection" props === True
          Map.member "limit"      props === True
          Map.member "cursor"     props === True
          Map.member "reverse"    props === True
          -- repo is a string
          case Map.lookup "repo" props of
            Just (LexPrimString s) ->
              lexStrFormat s === Just LexFmtAtIdentifier
            _ -> failure
          -- limit is an integer with bounds
          case Map.lookup "limit" props of
            Just (LexPrimInteger i) -> do
              lexIntMinimum i === Just 1
              lexIntMaximum i === Just 100
              lexIntDefault i === Just 50
            _ -> failure
          -- reverse is a boolean
          case Map.lookup "reverse" props of
            Just (LexPrimBoolean _) -> success
            _                       -> failure
      -- Has output
      case lexXrpcQueryOutput q of
        Nothing -> failure
        Just body -> lexXrpcBodyEncoding body === "application/json"
    _ -> failure

  -- "record" must be an object def
  case Map.lookup "record" (lexDocDefs doc) of
    Just (LexObject' obj) -> do
      Map.member "uri"   (lexObjProperties obj) === True
      Map.member "cid"   (lexObjProperties obj) === True
      Map.member "value" (lexObjProperties obj) === True
      -- uri is a string with at-uri format
      case Map.lookup "uri" (lexObjProperties obj) of
        Just (LexObjPrimString s) -> lexStrFormat s === Just LexFmtAtUri
        _                         -> failure
      -- value is unknown
      case Map.lookup "value" (lexObjProperties obj) of
        Just (LexObjPrimUnknown _) -> success
        _                          -> failure
    _ -> failure

-- | A minimal lexicon with a token def parses correctly.
prop_parseMinimal :: Property
prop_parseMinimal = withTests 1 . property $ do
  doc <- evalEither (Aeson.eitherDecode minimalJson)
  lexDocId doc      === "io.example.ping"
  lexDocLexicon doc === 1
  case Map.lookup "main" (lexDocDefs doc) of
    Just (LexToken' tok) -> lexTokenDescription tok === Just "A ping token."
    _                    -> failure

-- | Missing required field "lexicon" causes a parse failure.
prop_missingLexiconFieldFails :: Property
prop_missingLexiconFieldFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"id\": \"io.example.foo\"}"
  case (Aeson.eitherDecode bad :: Either String LexiconDoc) of
    Left  _ -> success
    Right _ -> failure

-- | Unknown "type" values cause a parse failure (not silently ignored).
prop_unknownTypeFails :: Property
prop_unknownTypeFails = withTests 1 . property $ do
  let bad = BLC.pack $ unlines
        [ "{\"lexicon\":1,\"id\":\"io.x\",\"defs\":{"
        , "  \"main\":{\"type\":\"notAType\"}"
        , "}}"
        ]
  case (Aeson.eitherDecode bad :: Either String LexiconDoc) of
    Left  _ -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Lexicon.Json"
  [ ("parse com.atproto.repo.listRecords", prop_parseListRecords)
  , ("parse minimal token lexicon",         prop_parseMinimal)
  , ("missing 'lexicon' field fails",       prop_missingLexiconFieldFails)
  , ("unknown 'type' value fails",          prop_unknownTypeFails)
  ]
