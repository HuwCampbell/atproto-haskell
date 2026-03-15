-- | Round-trip property tests for DAG-JSON encoding of 'LexValue'.
module Test.ATProto.Ipld.Json (tests) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import           Data.Int              (Int64)
import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import ATProto.Ipld.Value (Cid (..), BlobRef (..), LexValue (..))
import ATProto.Ipld.Json  (encodeLexJson, decodeLexJson,
                            lexValueToJson, jsonToLexValue)

-- ---------------------------------------------------------------------------
-- Generator
-- ---------------------------------------------------------------------------

genCid :: Gen Cid
genCid = Cid . T.pack <$> Gen.string (Range.linear 5 20) Gen.alphaNum

genBlobRef :: Gen BlobRef
genBlobRef =
    BlobRef
        <$> genCid
        <*> Gen.text (Range.linear 3 20) Gen.alphaNum
        <*> Gen.int64 (Range.linear 0 1000000)

genLexValue :: Gen LexValue
genLexValue = Gen.recursive Gen.choice nonRecursive recursive
  where
    nonRecursive =
        [ pure LexNull
        , LexBool   <$> Gen.bool
        , LexInt    <$> Gen.int64 Range.linearBounded
        , LexString <$> Gen.text (Range.linear 0 30) Gen.unicode
        , LexBytes  <$> Gen.bytes (Range.linear 0 20)
        , LexLink   <$> genCid
        , LexBlob   <$> genBlobRef
        ]
    recursive =
        [ LexArray  <$> Gen.list  (Range.linear 0 5) genLexValue
        , LexObject <$> Gen.map   (Range.linear 0 5)
                          (liftA2 (,)
                              (Gen.text (Range.linear 1 10) Gen.alphaNum)
                              genLexValue)
        ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | JSON round-trip: encode then decode should give back the original value.
prop_jsonRoundTrip :: Property
prop_jsonRoundTrip = property $ do
    v <- forAll genLexValue
    case decodeLexJson (encodeLexJson v) of
        Left  err -> annotate err >> failure
        Right v'  -> v === v'

-- | Aeson Value round-trip via lexValueToJson / jsonToLexValue.
prop_aesonRoundTrip :: Property
prop_aesonRoundTrip = property $ do
    v <- forAll genLexValue
    case jsonToLexValue (lexValueToJson v) of
        Left  err -> annotate err >> failure
        Right v'  -> v === v'

-- | Floats in JSON are rejected.
prop_floatRejected :: Property
prop_floatRejected = withTests 1 . property $ do
    let bad = "3.14"
    case decodeLexJson bad of
        Left  _ -> success
        Right _ -> failure

-- | CID links round-trip with the expected wrapper object.
prop_cidEncoding :: Property
prop_cidEncoding = withTests 1 . property $ do
    let cid = Cid "bafyreiabc123"
        encoded = encodeLexJson (LexLink cid)
        expected = "{\"$link\":\"bafyreiabc123\"}"
    encoded === expected

-- | Bytes round-trip through base64.
prop_bytesEncoding :: Property
prop_bytesEncoding = withTests 1 . property $ do
    let bs  = BC.pack "hello"
        lv  = LexBytes bs
    case decodeLexJson (encodeLexJson lv) of
        Left  err -> annotate err >> failure
        Right lv' -> lv === lv'

-- | Nested objects round-trip correctly.
prop_nestedObject :: Property
prop_nestedObject = withTests 1 . property $ do
    let v = LexObject (Map.fromList
                [ ("a", LexInt 1)
                , ("b", LexArray [LexNull, LexBool True])
                ])
    case decodeLexJson (encodeLexJson v) of
        Left  err -> annotate err >> failure
        Right v'  -> v === v'

-- | BlobRef round-trips through JSON.
prop_blobRefRoundTrip :: Property
prop_blobRefRoundTrip = property $ do
    b <- forAll genBlobRef
    case decodeLexJson (encodeLexJson (LexBlob b)) of
        Left  err -> annotate err >> failure
        Right v'  -> LexBlob b === v'

-- | BlobRef encodes to the expected JSON shape.
prop_blobRefEncoding :: Property
prop_blobRefEncoding = withTests 1 . property $ do
    let b = BlobRef (Cid "bafyreiabc123") "image/jpeg" 12345
        encoded = encodeLexJson (LexBlob b)
    case decodeLexJson encoded of
        Left  err -> annotate err >> failure
        Right v'  -> LexBlob b === v'

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Ipld.Json"
    [ ("JSON round-trip",     prop_jsonRoundTrip)
    , ("Aeson round-trip",    prop_aesonRoundTrip)
    , ("float rejected",      prop_floatRejected)
    , ("CID encoding",        prop_cidEncoding)
    , ("bytes encoding",      prop_bytesEncoding)
    , ("nested object",       prop_nestedObject)
    , ("BlobRef round-trip",  prop_blobRefRoundTrip)
    , ("BlobRef encoding",    prop_blobRefEncoding)
    ]
