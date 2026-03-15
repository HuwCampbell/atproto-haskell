-- | Round-trip property tests for DAG-CBOR encoding of 'LexValue'.
module Test.ATProto.Ipld.Cbor (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Data.Int        (Int64)
import           Hedgehog
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range

import ATProto.Ipld.Value (Cid (..), BlobRef (..), LexValue (..))
import ATProto.Ipld.Cbor  (encodeLexCbor, decodeLexCbor)

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
        [ LexArray  <$> Gen.list (Range.linear 0 5) genLexValue
        , LexObject <$> Gen.map  (Range.linear 0 5)
                          (liftA2 (,)
                              (Gen.text (Range.linear 1 10) Gen.alphaNum)
                              genLexValue)
        ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | CBOR round-trip: encode then decode gives back the original value.
prop_cborRoundTrip :: Property
prop_cborRoundTrip = property $ do
    v <- forAll genLexValue
    case decodeLexCbor (encodeLexCbor v) of
        Left  err -> annotate err >> failure
        Right v'  -> v === v'

-- | CID links survive a CBOR round-trip.
prop_cidRoundTrip :: Property
prop_cidRoundTrip = property $ do
    cid <- forAll genCid
    let v = LexLink cid
    case decodeLexCbor (encodeLexCbor v) of
        Left  err -> annotate err >> failure
        Right v'  -> v === v'

-- | Bytes survive a CBOR round-trip.
prop_bytesRoundTrip :: Property
prop_bytesRoundTrip = property $ do
    bs <- forAll (Gen.bytes (Range.linear 0 40))
    let v = LexBytes bs
    case decodeLexCbor (encodeLexCbor v) of
        Left  err -> annotate err >> failure
        Right v'  -> v === v'

-- | Nested objects survive a CBOR round-trip.
prop_nestedObject :: Property
prop_nestedObject = withTests 1 . property $ do
    let v = LexObject (Map.fromList
                [ ("x", LexInt 42)
                , ("y", LexArray [LexNull, LexBool False, LexString "hi"])
                ])
    case decodeLexCbor (encodeLexCbor v) of
        Left  err -> annotate err >> failure
        Right v'  -> v === v'

-- | BlobRef survives a CBOR round-trip.
prop_blobRefRoundTrip :: Property
prop_blobRefRoundTrip = property $ do
    b <- forAll genBlobRef
    case decodeLexCbor (encodeLexCbor (LexBlob b)) of
        Left  err -> annotate err >> failure
        Right v'  -> LexBlob b === v'

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Ipld.Cbor"
    [ ("CBOR round-trip",    prop_cborRoundTrip)
    , ("CID round-trip",     prop_cidRoundTrip)
    , ("bytes round-trip",   prop_bytesRoundTrip)
    , ("nested object",      prop_nestedObject)
    , ("BlobRef round-trip", prop_blobRefRoundTrip)
    ]
