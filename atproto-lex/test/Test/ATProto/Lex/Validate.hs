-- | Tests for 'ATProto.Lex.Validate'.
--
-- Each test parses an inline lexicon JSON document, builds a codec that
-- either matches or mismatches, then asserts the validation result.
module Test.ATProto.Lex.Validate (tests) where

import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import qualified ATProto.Lex.Codec       as Codec
import           ATProto.Lex.Schema      (LexSchema (..), LexFormat (..))
import           ATProto.Lex.Validate
import           ATProto.Lexicon         (LexiconDoc)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A simple object lexicon with three fields.
simpleObjectJson :: BLC.ByteString
simpleObjectJson = BLC.pack $ unlines
  [ "{"
  , "  \"lexicon\": 1,"
  , "  \"id\": \"com.example.simpleObject\","
  , "  \"defs\": {"
  , "    \"main\": {"
  , "      \"type\": \"object\","
  , "      \"required\": [\"uri\", \"cid\"],"
  , "      \"properties\": {"
  , "        \"uri\":    { \"type\": \"string\", \"format\": \"at-uri\" },"
  , "        \"cid\":    { \"type\": \"string\", \"format\": \"cid\" },"
  , "        \"value\":  { \"type\": \"unknown\" }"
  , "      }"
  , "    }"
  , "  }"
  , "}"
  ]

-- | A query lexicon with an output body.
queryJson :: BLC.ByteString
queryJson = BLC.pack $ unlines
  [ "{"
  , "  \"lexicon\": 1,"
  , "  \"id\": \"com.example.query\","
  , "  \"defs\": {"
  , "    \"main\": {"
  , "      \"type\": \"query\","
  , "      \"output\": {"
  , "        \"encoding\": \"application/json\","
  , "        \"schema\": {"
  , "          \"type\": \"object\","
  , "          \"required\": [\"items\"],"
  , "          \"properties\": {"
  , "            \"cursor\": { \"type\": \"string\" },"
  , "            \"items\":  {"
  , "              \"type\": \"array\","
  , "              \"items\": { \"type\": \"string\", \"format\": \"at-uri\" }"
  , "            }"
  , "          }"
  , "        }"
  , "      }"
  , "    }"
  , "  }"
  , "}"
  ]

-- | A lexicon with a reference to a sub-def.
refJson :: BLC.ByteString
refJson = BLC.pack $ unlines
  [ "{"
  , "  \"lexicon\": 1,"
  , "  \"id\": \"com.example.withRef\","
  , "  \"defs\": {"
  , "    \"main\": {"
  , "      \"type\": \"query\","
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
  , "      \"required\": [\"uri\", \"value\"],"
  , "      \"properties\": {"
  , "        \"uri\":   { \"type\": \"string\", \"format\": \"at-uri\" },"
  , "        \"value\": { \"type\": \"unknown\" }"
  , "      }"
  , "    }"
  , "  }"
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseLexicon :: BLC.ByteString -> PropertyT IO LexiconDoc
parseLexicon bs = evalEither (Aeson.eitherDecode bs)

-- ---------------------------------------------------------------------------
-- Properties: matching codecs (should produce no errors)
-- ---------------------------------------------------------------------------

-- | A codec that exactly matches the simple object lexicon.
prop_matchingObject :: Property
prop_matchingObject = withTests 1 . property $ do
  doc <- parseLexicon simpleObjectJson
  let codec = Codec.record "com.example.simpleObject" $
                (,,)
                  <$> Codec.requiredField "uri"   Codec.atUri    (\(a,_,_) -> a)
                  <*> Codec.requiredField "cid"   (Codec.string LexFormatCid) (\(_,b,_) -> b)
                  <*> Codec.optionalField "value" Codec.lexValue (\(_,_,c) -> c)
  validate doc "main" (Codec.schema codec) === []

-- | A query codec that exactly matches the query lexicon.
prop_matchingQuery :: Property
prop_matchingQuery = withTests 1 . property $ do
  doc <- parseLexicon queryJson
  let codec = Codec.record "com.example.query#response" $
                (,)
                  <$> Codec.optionalField "cursor" Codec.text    fst
                  <*> Codec.requiredField "items"  (Codec.array Codec.atUri) snd
  validate doc "main" (Codec.schema codec) === []

-- ---------------------------------------------------------------------------
-- Properties: mismatching codecs (should produce errors)
-- ---------------------------------------------------------------------------

-- | A missing field in the codec → FieldExtra.
prop_missingFieldInCodec :: Property
prop_missingFieldInCodec = withTests 1 . property $ do
  doc <- parseLexicon simpleObjectJson
  -- Codec only has "uri" and "cid", missing "value"
  let codec = Codec.record "com.example.simpleObject" $
                (,)
                  <$> Codec.requiredField "uri" Codec.atUri    fst
                  <*> Codec.requiredField "cid" (Codec.string LexFormatCid) snd
  let errors = validate doc "main" (Codec.schema codec)
  -- Should have at least one FieldExtra for "value"
  assert (any isFieldExtra errors)
  where
    isFieldExtra (FieldExtra _ "value") = True
    isFieldExtra _                      = False

-- | An extra field in the codec → FieldMissing.
prop_extraFieldInCodec :: Property
prop_extraFieldInCodec = withTests 1 . property $ do
  doc <- parseLexicon simpleObjectJson
  let codec = Codec.record "com.example.simpleObject" $
                (,,,)
                  <$> Codec.requiredField "uri"   Codec.atUri    (\(a,_,_,_) -> a)
                  <*> Codec.requiredField "cid"   (Codec.string LexFormatCid)  (\(_,b,_,_) -> b)
                  <*> Codec.requiredField "value" Codec.lexValue (\(_,_,c,_) -> c)
                  <*> Codec.requiredField "bogus" Codec.text     (\(_,_,_,d) -> d)
  let errors = validate doc "main" (Codec.schema codec)
  assert (any isFieldMissing errors)
  where
    isFieldMissing (FieldMissing _ "bogus") = True
    isFieldMissing _                        = False

-- | Wrong type for a field → TypeMismatch.
prop_wrongFieldType :: Property
prop_wrongFieldType = withTests 1 . property $ do
  doc <- parseLexicon simpleObjectJson
  -- Use Codec.bool for "uri" which should be string/at-uri
  let codec = Codec.record "com.example.simpleObject" $
                (,,)
                  <$> Codec.requiredField "uri"   Codec.bool     (\(a,_,_) -> a)
                  <*> Codec.requiredField "cid"   (Codec.string LexFormatCid)  (\(_,b,_) -> b)
                  <*> Codec.requiredField "value" Codec.lexValue (\(_,_,c) -> c)
  let errors = validate doc "main" (Codec.schema codec)
  assert (any isTypeMismatch errors)
  where
    isTypeMismatch (TypeMismatch _ _ _) = True
    isTypeMismatch _                    = False

-- | Required field marked as optional in codec → RequiredMismatch.
prop_requiredMismatch :: Property
prop_requiredMismatch = withTests 1 . property $ do
  doc <- parseLexicon simpleObjectJson
  -- "uri" is required in lexicon but we use optionalField
  let codec = Codec.record "com.example.simpleObject" $
                (,,)
                  <$> Codec.optionalField "uri"   Codec.atUri    (\(a,_,_) -> a)
                  <*> Codec.requiredField "cid"   (Codec.string LexFormatCid)  (\(_,b,_) -> b)
                  <*> Codec.requiredField "value" Codec.lexValue (\(_,_,c) -> c)
  let errors = validate doc "main" (Codec.schema codec)
  assert (any isRequiredMismatch errors)
  where
    isRequiredMismatch (RequiredMismatch _ "uri" True False) = True
    isRequiredMismatch _                                     = False

-- | Definition not found → DefinitionNotFound.
prop_defNotFound :: Property
prop_defNotFound = withTests 1 . property $ do
  doc <- parseLexicon simpleObjectJson
  let errors = validate doc "nonexistent" LexSchemaNull
  assert (any isDefNotFound errors)
  where
    isDefNotFound (DefinitionNotFound _ "nonexistent") = True
    isDefNotFound _                                    = False

-- | String format mismatch.
prop_wrongFormat :: Property
prop_wrongFormat = withTests 1 . property $ do
  doc <- parseLexicon simpleObjectJson
  -- Use did format for "uri" which should be at-uri
  let codec = Codec.record "com.example.simpleObject" $
                (,,)
                  <$> Codec.requiredField "uri"   Codec.did      (\(a,_,_) -> a)
                  <*> Codec.requiredField "cid"   (Codec.string LexFormatCid)  (\(_,b,_) -> b)
                  <*> Codec.requiredField "value" Codec.lexValue (\(_,_,c) -> c)
  let errors = validate doc "main" (Codec.schema codec)
  assert (any isFormatMismatch errors)
  where
    isFormatMismatch (TypeMismatch ["uri"] "string/at-uri" "string/did") = True
    isFormatMismatch _                                                   = False

-- | Array with wrong element type → TypeMismatch.
prop_arrayElementMismatch :: Property
prop_arrayElementMismatch = withTests 1 . property $ do
  doc <- parseLexicon queryJson
  -- Use array of int instead of array of at-uri strings
  let codec = Codec.record "com.example.query#response" $
                (,)
                  <$> Codec.optionalField "cursor" Codec.text fst
                  <*> Codec.requiredField "items"  (Codec.array Codec.int) snd
  let errors = validate doc "main" (Codec.schema codec)
  assert (any isTypeMismatch errors)
  where
    isTypeMismatch (TypeMismatch _ _ _) = True
    isTypeMismatch _                    = False

-- | Ref resolution works — codec that matches resolved object.
prop_refResolution :: Property
prop_refResolution = withTests 1 . property $ do
  doc <- parseLexicon refJson
  -- Build a codec matching the "record" sub-def
  let recordCodec = Codec.record "com.example.withRef#record" $
                      (,)
                        <$> Codec.requiredField "uri"   Codec.atUri    fst
                        <*> Codec.requiredField "value" Codec.lexValue snd
  -- Validate the "record" def directly
  validate doc "record" (Codec.schema recordCodec) === []

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Lex.Validate"
  [ ("matching object",        prop_matchingObject)
  , ("matching query",         prop_matchingQuery)
  , ("missing field in codec", prop_missingFieldInCodec)
  , ("extra field in codec",   prop_extraFieldInCodec)
  , ("wrong field type",       prop_wrongFieldType)
  , ("required mismatch",      prop_requiredMismatch)
  , ("def not found",          prop_defNotFound)
  , ("wrong string format",    prop_wrongFormat)
  , ("array element mismatch", prop_arrayElementMismatch)
  , ("ref resolution",         prop_refResolution)
  ]
