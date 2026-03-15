-- | Property tests for the 'ATProto.Lex.Codec' combinators.
--
-- Covers:
--
--   1. Round-trip JSON for each primitive codec.
--   2. Round-trip CBOR for each primitive codec.
--   3. Record codec with required and optional fields.
--   4. Union codec with @$type@ dispatch.
--   5. Schema embedding.
--   6. Error cases: 'MissingField' and 'TypeMismatch'.
module Test.ATProto.Lex.Codec (tests) where

import qualified Data.Map.Strict   as Map
import qualified Data.Text         as T
import           Data.Int          (Int64)
import           Hedgehog
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range

import           ATProto.Ipld.Value (LexValue (..), BlobRef (..), Cid (..))
import qualified ATProto.Lex.Codec  as Codec
import qualified ATProto.Lex.Json   as LexJson
import qualified ATProto.Lex.Cbor   as LexCbor
import           ATProto.Lex.Schema (LexSchema (..))

-- ---------------------------------------------------------------------------
-- Example record type used throughout
-- ---------------------------------------------------------------------------

data StrongRef = StrongRef
  { srUri :: T.Text
  , srCid :: T.Text
  } deriving (Eq, Show)

strongRefCodec :: Codec.Codec StrongRef
strongRefCodec =
    Codec.record "com.atproto.repo.strongRef" $
        StrongRef
            <$> Codec.requiredField "uri" Codec.atUri srUri
            <*> Codec.requiredField "cid" Codec.text  srCid

-- ---------------------------------------------------------------------------
-- Example union type
-- ---------------------------------------------------------------------------

data MyUnion
  = MyText T.Text
  | MyInt  Int64
  deriving (Eq, Show)

myUnionCodec :: Codec.Codec MyUnion
myUnionCodec = Codec.union
    [ Codec.unionVariant "example.text"
        (Codec.record "example.text" $
            id <$> Codec.requiredField "value" Codec.text id)
        (\u -> case u of { MyText t -> Just t; _ -> Nothing })
        MyText
    , Codec.unionVariant "example.int"
        (Codec.record "example.int" $
            id <$> Codec.requiredField "value" Codec.int id)
        (\u -> case u of { MyInt n -> Just n; _ -> Nothing })
        MyInt
    ]

-- ---------------------------------------------------------------------------
-- 1. JSON round-trips for primitives
-- ---------------------------------------------------------------------------

prop_jsonBool :: Property
prop_jsonBool = property $ do
    b <- forAll Gen.bool
    roundTripJson Codec.bool b

prop_jsonInt :: Property
prop_jsonInt = property $ do
    n <- forAll (Gen.int64 Range.linearBounded)
    roundTripJson Codec.int n

prop_jsonText :: Property
prop_jsonText = property $ do
    t <- forAll (Gen.text (Range.linear 0 40) Gen.unicode)
    roundTripJson Codec.text t

prop_jsonBytes :: Property
prop_jsonBytes = property $ do
    bs <- forAll (Gen.bytes (Range.linear 0 20))
    roundTripJson Codec.bytes bs

prop_jsonNull :: Property
prop_jsonNull = withTests 1 . property $ roundTripJson Codec.null ()

-- ---------------------------------------------------------------------------
-- 2. CBOR round-trips for primitives
-- ---------------------------------------------------------------------------

prop_cborBool :: Property
prop_cborBool = property $ do
    b <- forAll Gen.bool
    roundTripCbor Codec.bool b

prop_cborInt :: Property
prop_cborInt = property $ do
    n <- forAll (Gen.int64 Range.linearBounded)
    roundTripCbor Codec.int n

prop_cborText :: Property
prop_cborText = property $ do
    t <- forAll (Gen.text (Range.linear 0 40) Gen.unicode)
    roundTripCbor Codec.text t

prop_cborBytes :: Property
prop_cborBytes = property $ do
    bs <- forAll (Gen.bytes (Range.linear 0 20))
    roundTripCbor Codec.bytes bs

-- ---------------------------------------------------------------------------
-- 3. Record codec round-trips
-- ---------------------------------------------------------------------------

genStrongRef :: Gen StrongRef
genStrongRef =
    StrongRef
        <$> Gen.text (Range.linear 5 30) Gen.alphaNum
        <*> Gen.text (Range.linear 5 30) Gen.alphaNum

prop_recordJsonRoundTrip :: Property
prop_recordJsonRoundTrip = property $ do
    ref <- forAll genStrongRef
    roundTripJson strongRefCodec ref

prop_recordCborRoundTrip :: Property
prop_recordCborRoundTrip = property $ do
    ref <- forAll genStrongRef
    roundTripCbor strongRefCodec ref

-- | Record with optional field.
data WithOpt = WithOpt
  { woRequired :: T.Text
  , woOptional :: Maybe T.Text
  } deriving (Eq, Show)

withOptCodec :: Codec.Codec WithOpt
withOptCodec =
    Codec.record "example.withOpt" $
        WithOpt
            <$> Codec.requiredField "req" Codec.text woRequired
            <*> Codec.optionalField "opt" Codec.text woOptional

prop_optionalFieldPresent :: Property
prop_optionalFieldPresent = withTests 1 . property $ do
    roundTripJson withOptCodec (WithOpt "hello" (Just "world"))

prop_optionalFieldAbsent :: Property
prop_optionalFieldAbsent = withTests 1 . property $ do
    roundTripJson withOptCodec (WithOpt "hello" Nothing)

-- ---------------------------------------------------------------------------
-- 4. Union codec
-- ---------------------------------------------------------------------------

prop_unionTextRoundTrip :: Property
prop_unionTextRoundTrip = property $ do
    t <- forAll (Gen.text (Range.linear 1 20) Gen.alphaNum)
    roundTripJson myUnionCodec (MyText t)

prop_unionIntRoundTrip :: Property
prop_unionIntRoundTrip = property $ do
    n <- forAll (Gen.int64 (Range.linear (-100) 100))
    roundTripJson myUnionCodec (MyInt n)

-- | Text variant encodes with @$type = "example.text"@.
prop_unionTextTypeTag :: Property
prop_unionTextTypeTag = withTests 1 . property $ do
    let lv = Codec.writer myUnionCodec (MyText "hello")
    case lv of
        LexObject m ->
            Map.lookup "$type" m === Just (LexString "example.text")
        _ -> failure

-- | Int variant encodes with @$type = "example.int"@.
prop_unionIntTypeTag :: Property
prop_unionIntTypeTag = withTests 1 . property $ do
    let lv = Codec.writer myUnionCodec (MyInt 42)
    case lv of
        LexObject m ->
            Map.lookup "$type" m === Just (LexString "example.int")
        _ -> failure

-- ---------------------------------------------------------------------------
-- 5. Schema embedding
-- ---------------------------------------------------------------------------

prop_primitiveSchemas :: Property
prop_primitiveSchemas = withTests 1 . property $ do
    Codec.schema Codec.null       === LexSchemaNull
    Codec.schema Codec.bool       === LexSchemaBool
    Codec.schema Codec.int        === LexSchemaInt
    Codec.schema Codec.text       === LexSchemaString Nothing
    Codec.schema Codec.bytes      === LexSchemaBytes
    Codec.schema Codec.lexValue   === LexSchemaUnknown

prop_recordSchema :: Property
prop_recordSchema = withTests 1 . property $ do
    case Codec.schema strongRefCodec of
        LexSchemaObject fields -> length fields === 2
        _                      -> failure

prop_arraySchema :: Property
prop_arraySchema = withTests 1 . property $ do
    case Codec.schema (Codec.array Codec.int) of
        LexSchemaArray LexSchemaInt -> success
        _                           -> failure

-- ---------------------------------------------------------------------------
-- 6. Error cases
-- ---------------------------------------------------------------------------

prop_missingField :: Property
prop_missingField = withTests 1 . property $ do
    let bad = LexObject Map.empty
    case Codec.decoder strongRefCodec bad of
        Left (Codec.MissingField f) -> f === "uri"
        _                           -> failure

prop_typeMismatch :: Property
prop_typeMismatch = withTests 1 . property $ do
    case Codec.decoder Codec.bool LexNull of
        Left (Codec.TypeMismatch t _) -> t === "bool"
        _                             -> failure

prop_intTypeMismatch :: Property
prop_intTypeMismatch = withTests 1 . property $ do
    case Codec.decoder Codec.int (LexString "not-a-number") of
        Left (Codec.TypeMismatch t _) -> t === "int"
        _                             -> failure

-- ---------------------------------------------------------------------------
-- 7. Blob codec
-- ---------------------------------------------------------------------------

genBlobRef :: Gen BlobRef
genBlobRef =
    BlobRef
        <$> (Cid <$> Gen.text (Range.linear 5 20) Gen.alphaNum)
        <*> Gen.text (Range.linear 3 20) Gen.alphaNum
        <*> Gen.int64 (Range.linear 0 1000000)

prop_jsonBlobRoundTrip :: Property
prop_jsonBlobRoundTrip = property $ do
    b <- forAll genBlobRef
    roundTripJson Codec.blob b

prop_cborBlobRoundTrip :: Property
prop_cborBlobRoundTrip = property $ do
    b <- forAll genBlobRef
    roundTripCbor Codec.blob b

prop_blobSchema :: Property
prop_blobSchema = withTests 1 . property $
    Codec.schema Codec.blob === LexSchemaBlob

prop_blobTypeMismatch :: Property
prop_blobTypeMismatch = withTests 1 . property $ do
    case Codec.decoder Codec.blob (LexString "not-a-blob") of
        Left (Codec.TypeMismatch t _) -> t === "blob"
        _                             -> failure

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

roundTripJson :: (Eq a, Show a) => Codec.Codec a -> a -> PropertyT IO ()
roundTripJson c a = do
    let encoded = LexJson.encode c a
    case LexJson.decode c encoded of
        Left  err -> annotate err >> failure
        Right a'  -> a === a'

roundTripCbor :: (Eq a, Show a) => Codec.Codec a -> a -> PropertyT IO ()
roundTripCbor c a = do
    let encoded = LexCbor.serialise c a
    case LexCbor.deserialise c encoded of
        Left  err -> annotate (show err) >> failure
        Right a'  -> a === a'

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Lex.Codec"
    [ -- JSON primitives
      ("JSON bool",                   prop_jsonBool)
    , ("JSON int",                    prop_jsonInt)
    , ("JSON text",                   prop_jsonText)
    , ("JSON bytes",                  prop_jsonBytes)
    , ("JSON null",                   prop_jsonNull)
      -- CBOR primitives
    , ("CBOR bool",                   prop_cborBool)
    , ("CBOR int",                    prop_cborInt)
    , ("CBOR text",                   prop_cborText)
    , ("CBOR bytes",                  prop_cborBytes)
      -- Records
    , ("record JSON round-trip",      prop_recordJsonRoundTrip)
    , ("record CBOR round-trip",      prop_recordCborRoundTrip)
    , ("optional field present",      prop_optionalFieldPresent)
    , ("optional field absent",       prop_optionalFieldAbsent)
      -- Union
    , ("union text JSON",             prop_unionTextRoundTrip)
    , ("union int JSON",              prop_unionIntRoundTrip)
    , ("union text $type tag",        prop_unionTextTypeTag)
    , ("union int $type tag",         prop_unionIntTypeTag)
      -- Schema
    , ("primitive schemas",           prop_primitiveSchemas)
    , ("record schema",               prop_recordSchema)
    , ("array schema",                prop_arraySchema)
      -- Errors
    , ("missing field",               prop_missingField)
    , ("type mismatch bool",          prop_typeMismatch)
    , ("type mismatch int",           prop_intTypeMismatch)
      -- Blob codec
    , ("JSON blob round-trip",        prop_jsonBlobRoundTrip)
    , ("CBOR blob round-trip",        prop_cborBlobRoundTrip)
    , ("blob schema",                 prop_blobSchema)
    , ("blob type mismatch",          prop_blobTypeMismatch)
    ]
