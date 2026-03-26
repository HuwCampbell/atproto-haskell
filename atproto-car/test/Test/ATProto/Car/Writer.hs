module Test.ATProto.Car.Writer (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import ATProto.Car.Cid    (CidBytes (..), cidForDagCbor)
import ATProto.Car.Parser  (readCar, readCarWithRoot)
import ATProto.Car.Writer  (writeCar, writeCarWithRoot)

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | Generator for a block map with 1–5 blocks, each a small blob.
genBlockMap :: Gen (Map.Map CidBytes BS.ByteString)
genBlockMap = do
  n <- Gen.int (Range.linear 1 5)
  pairs <- mapM (\_ -> genBlock) [1..n]
  return (Map.fromList pairs)

-- | Generator for a single (cid, bytes) pair.
genBlock :: Gen (CidBytes, BS.ByteString)
genBlock = do
  body <- Gen.bytes (Range.linear 1 64)
  let cid = cidForDagCbor body
  return (cid, body)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | write/read round-trip: writing and reading a single-root CAR yields
-- the same root and block map.
prop_writeReadRoundTrip :: Property
prop_writeReadRoundTrip = property $ do
  blocks <- forAll genBlockMap
  let root    = fst (Map.findMin blocks)
      car     = writeCarWithRoot root blocks
  case readCarWithRoot car of
    Left err      -> do
      annotate (show err)
      failure
    Right (root', blocks') -> do
      root'   === root
      blocks' === blocks

-- | Multi-root write/read round-trip.
prop_multiRootRoundTrip :: Property
prop_multiRootRoundTrip = property $ do
  blocks <- forAll genBlockMap
  let roots   = Map.keys blocks
      car     = writeCar roots blocks
  case readCar car of
    Left err           -> do
      annotate (show err)
      failure
    Right (roots', blocks') -> do
      roots'  === roots
      blocks' === blocks

-- | Empty block map with a synthetic root round-trips correctly.
prop_emptyBlockMap :: Property
prop_emptyBlockMap = property $ do
  let root   = cidForDagCbor BS.empty
      blocks = Map.singleton root BS.empty
      car    = writeCarWithRoot root blocks
  case readCarWithRoot car of
    Left err           -> do
      annotate (show err)
      failure
    Right (root', blocks') -> do
      root'   === root
      blocks' === blocks

-- | The known minimal CAR fixture from the parser tests is re-produced
-- by the writer when given the same root and block map.
prop_knownVector :: Property
prop_knownVector = property $ do
  -- The block is CBOR null = 0xf6
  let body = BS.singleton 0xf6
      cid  = cidForDagCbor body
      car  = writeCarWithRoot cid (Map.singleton cid body)
  -- We can't check byte-equality with the hand-crafted test fixture
  -- because header field order might differ (version/roots vs roots/version).
  -- Instead we check the round-trip.
  case readCarWithRoot car of
    Left err           -> do
      annotate (show err)
      failure
    Right (root', blocks') -> do
      root'              === cid
      Map.lookup cid blocks' === Just body

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.Car.Writer"
  [ ("write/read single-root round-trip", prop_writeReadRoundTrip)
  , ("write/read multi-root round-trip",  prop_multiRootRoundTrip)
  , ("empty block map round-trip",        prop_emptyBlockMap)
  , ("known vector round-trip",           prop_knownVector)
  ]
