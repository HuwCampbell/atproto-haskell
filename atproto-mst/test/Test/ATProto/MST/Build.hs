-- | Property-based tests for MST building and round-tripping.
module Test.ATProto.MST.Build (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List.NonEmpty   as NE
import           Data.List.NonEmpty   (NonEmpty)

import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.Car.Cid      (CidBytes, parseCidFromBytes, unsafeRawCid)
import ATProto.Car.Parser   (readCarWithRoot)
import ATProto.MST.Node     (NodeData (..), TreeEntry (..), decodeNode)
import ATProto.MST.Encode   (encodeNode)
import ATProto.MST.Tree
import qualified ATProto.MST.Tree as Tree

import Prelude hiding (lookup)

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | Generate a valid MST key character.
genKeyChar :: Gen Char
genKeyChar = Gen.element (['a'..'z'] ++ ['0'..'9'])

-- | Generate a valid MST key in the form @\"collection\/rkey\"@.
genMstKey :: Gen T.Text
genMstKey = do
  col  <- Gen.text (Range.linear 1 20) genKeyChar
  rkey <- Gen.text (Range.linear 1 20) genKeyChar
  return (col <> "/" <> rkey)

-- | Generate a plausible CIDv1 (dag-cbor + sha2-256, 36 bytes).
genCidBytes :: Gen CidBytes
genCidBytes = do
  digest <- Gen.bytes (Range.singleton 32)
  let header = BS.pack [0x01, 0x71, 0x12, 0x20]
  return (unsafeRawCid (header <> digest))

-- | Generate a sorted, unique non-empty list of MST entries.
genSomeEntries :: Gen (NonEmpty (T.Text, CidBytes))
genSomeEntries = do
  n     <- Gen.int (Range.linear 1 100)
  pairs <- sequence [ (,) <$> genMstKey <*> genCidBytes | _ <- [1..n] ]
  return $ NE.fromList (Map.toAscList (Map.fromList pairs))

-- | Generate a sorted, unique list of MST entries.
genEntries :: Gen [(T.Text, CidBytes)]
genEntries = do
  n     <- Gen.int (Range.linear 0 100)
  pairs <- sequence [ (,) <$> genMstKey <*> genCidBytes | _ <- [1..n] ]
  return (Map.toAscList (Map.fromList pairs))

-- ---------------------------------------------------------------------------
-- Known fixture
-- ---------------------------------------------------------------------------

-- | The single-entry CAR file from the existing test suite.
singleEntryCar :: BS.ByteString
singleEntryCar = BS.pack
  [ 0x3a, 0xa2, 0x67, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x01, 0x65, 0x72, 0x6f, 0x6f, 0x74
  , 0x73, 0x81, 0xd8, 0x2a, 0x58, 0x25, 0x00, 0x01, 0x71, 0x12, 0x20, 0xce, 0x9c, 0xd4, 0xc3, 0x61
  , 0x25, 0xff, 0x0d, 0x5a, 0x10, 0x88, 0x85, 0x2e, 0xdc, 0x44, 0x5f, 0xdc, 0x08, 0xc1, 0xaf, 0x98
  , 0xf5, 0x01, 0x4e, 0x49, 0xf1, 0x9b, 0x06, 0x59, 0x1e, 0xf0, 0x76, 0x81, 0x01, 0x01, 0x71, 0x12
  , 0x20, 0xce, 0x9c, 0xd4, 0xc3, 0x61, 0x25, 0xff, 0x0d, 0x5a, 0x10, 0x88, 0x85, 0x2e, 0xdc, 0x44
  , 0x5f, 0xdc, 0x08, 0xc1, 0xaf, 0x98, 0xf5, 0x01, 0x4e, 0x49, 0xf1, 0x9b, 0x06, 0x59, 0x1e, 0xf0
  , 0x76, 0xa2, 0x61, 0x6c, 0xf6, 0x61, 0x65, 0x81, 0xa4, 0x61, 0x70, 0x00, 0x61, 0x6b, 0x58, 0x20
  , 0x63, 0x6f, 0x6d, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2e, 0x72, 0x65, 0x63, 0x6f
  , 0x72, 0x64, 0x2f, 0x33, 0x6a, 0x71, 0x66, 0x63, 0x71, 0x7a, 0x6d, 0x33, 0x66, 0x6e, 0x32, 0x6a
  , 0x61, 0x76, 0xd8, 0x2a, 0x58, 0x25, 0x00, 0x01, 0x71, 0x12, 0x20, 0xb0, 0xb2, 0x98, 0x8b, 0x6b
  , 0xbe, 0x72, 0x4b, 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde, 0x0b, 0xc7, 0xda, 0xe4, 0x1c
  , 0x46, 0xb4, 0x21, 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13, 0x61, 0x74, 0xf6, 0x25, 0x01
  , 0x71, 0x12, 0x20, 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b, 0xac, 0xda, 0x5e, 0x9e, 0x52
  , 0x47, 0x36, 0xde, 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21, 0x3c, 0x50, 0xe1, 0xd3, 0x5d
  , 0x4e, 0x5f, 0x13, 0xf6
  ]

-- | Leaf CID bytes (sha256 of CBOR null).
leafCidBytes :: BS.ByteString
leafCidBytes = BS.pack
  [ 0x01, 0x71, 0x12, 0x20
  , 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b
  , 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde
  , 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21
  , 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13
  ]

parsedLeafCid :: CidBytes
parsedLeafCid = case parseCidFromBytes leafCidBytes 0 of
  Right (c, _) -> c
  Left  err    -> error ("parsedLeafCid: " ++ err)

-- ---------------------------------------------------------------------------
-- Properties: encode/decode round-trip
-- ---------------------------------------------------------------------------

-- | Encoding then decoding a 'NodeData' yields the original value.
prop_encodeDecodeRoundTrip :: Property
prop_encodeDecodeRoundTrip = property $ do
  nd <- forAll genNodeData
  let encoded = encodeNode nd
  case decodeNode encoded of
    Left err  -> do
      annotate err
      failure
    Right nd' -> nd' === nd

-- | Generator for 'NodeData' with valid shared-prefix encoding.
genNodeData :: Gen NodeData
genNodeData = do
  hasLeft <- Gen.bool
  mLeft   <- if hasLeft then Just <$> genCidBytes else return Nothing
  n       <- Gen.int (Range.linear 0 10)
  entries <- genTreeEntries n BS.empty
  return (NodeData mLeft entries)

-- | Generate a list of 'TreeEntry's with correct shared-prefix encoding
--   relative to the accumulating previous key.
genTreeEntries :: Int -> BS.ByteString -> Gen [TreeEntry]
genTreeEntries 0 _ = return []
genTreeEntries n prevKey = do
  -- Generate a suffix that extends beyond the previous key.
  suffixPart <- BSC.pack <$> Gen.list (Range.linear 1 15) genKeyChar
  prefixLen  <- Gen.int (Range.linear 0 (BS.length prevKey))
  let shared  = BS.take prefixLen prevKey
      fullKey = shared <> suffixPart
      suffix  = BS.drop prefixLen fullKey
  val     <- genCidBytes
  hasRight <- Gen.bool
  mRight  <- if hasRight then Just <$> genCidBytes else return Nothing
  rest    <- genTreeEntries (n - 1) fullKey
  return (TreeEntry prefixLen suffix val mRight : rest)

-- ---------------------------------------------------------------------------
-- Properties: build / new Tree API
-- ---------------------------------------------------------------------------

-- | Empty input produces Nothing.
prop_buildEmpty :: Property
prop_buildEmpty = property $
  fromList [] === Nothing

-- | Building an MST then diffing from empty returns all entries as
--   'WCreate' in the original order.
prop_buildDiffRoundTrip :: Property
prop_buildDiffRoundTrip = property $ do
  entries <- forAll genEntries
  case fromList entries of
    Nothing  -> entries === []
    Just mst -> do
      let writes = Tree.diff Nothing mst
      let recovered = [ (wdKey w, wdCid w) | w <- writes ]
      recovered === entries

-- | Every key inserted into the MST can be retrieved with 'lookup'.
prop_buildGetRoundTrip :: Property
prop_buildGetRoundTrip = property $ do
  entries <- forAll genEntries
  case fromList entries of
    Nothing  -> entries === []
    Just mst ->
      mapM_ (checkLookup mst) entries
  where
    checkLookup mst (key, val) =
      case lookup key mst of
        Nothing -> do
          annotate ("key not found: " ++ T.unpack key)
          failure
        Just v  -> v === val

-- | Building an MST from the same entries twice produces the same root CID
--   (deterministic construction).
prop_buildDeterministic :: Property
prop_buildDeterministic = property $ do
  entries <- forAll genEntries
  fmap mstCid (fromList entries) === fmap mstCid (fromList entries)

-- ---------------------------------------------------------------------------
-- Properties: known test vector
-- ---------------------------------------------------------------------------

-- | Building an MST with the single-entry from the test fixture
--   produces the same root CID as the known CAR file.
prop_knownSingleEntry :: Property
prop_knownSingleEntry = property $ do
  case readCarWithRoot singleEntryCar of
    Left err -> do
      annotate (show err)
      failure
    Right (expectedRoot, _) -> do
      let key = "com.example.record/3jqfcqzm3fn2j" :: T.Text
      case fromList [(key, parsedLeafCid)] of
        Nothing  -> do
          annotate "fromList returned Nothing"
          failure
        Just mst -> mstCid mst === expectedRoot

-- | A missing key returns Nothing even in a non-empty tree.
prop_buildGetMissing :: Property
prop_buildGetMissing = property $ do
  entries <- forAll (Gen.filter (not . null) genEntries)
  case fromList entries of
    Nothing  -> failure
    Just mst -> do
      let missingKey = "zzz.missing/nothere"
      lookup missingKey mst === Nothing

-- ---------------------------------------------------------------------------
-- Properties: insert
-- ---------------------------------------------------------------------------

-- Oracle: what should insert give us?
-- Insert (key, val) into a sorted list, replacing any existing entry.
insertSorted :: T.Text -> CidBytes -> [(T.Text, CidBytes)] -> [(T.Text, CidBytes)]
insertSorted k v entries =
  Map.toAscList (Map.insert k v (Map.fromList entries))

-- | After 'insert', 'lookup' finds the new value.
prop_insertLookup :: Property
prop_insertLookup = property $ do
  entries <- forAll genEntries
  key     <- forAll genMstKey
  val     <- forAll genCidBytes
  -- Build initial tree (or use a single-entry tree if entries is empty)
  let base = case fromList entries of
               Just mst -> mst
               Nothing  -> singleton key val
  let mst' = insert key val base
  lookup key mst' === Just val

-- | 'insert' on a non-empty tree leaves all OTHER keys unchanged.
prop_insertPreservesOthers :: Property
prop_insertPreservesOthers = property $ do
  entries   <- forAll genSomeEntries
  key       <- forAll genMstKey
  val       <- forAll genCidBytes
  let mst'   = insert key val (fromNonEmpty entries)
      others = NE.filter (\(k, _) -> k /= key) entries
  mapM_ (\(k, v) -> lookup k mst' === Just v) others

-- | Inserting all entries one-by-one into the first entry yields the same
--   tree as building via 'fromList' (the oracle for structural correctness).
prop_insertMatchesFromList :: Property
prop_insertMatchesFromList = property $ do
  entries <- forAll genSomeEntries
  let (k0, v0) NE.:| rest = entries
      initial  = singleton k0 v0
      byInsert = foldr (uncurry insert) initial rest
  -- Build via fromList (the reference)
  let ref = fromNonEmpty entries
  toList byInsert === toList ref
  mstCid byInsert === mstCid ref

-- | 'insert' matches the naive oracle: toList . insert k v
--   == insertSorted k v . toList.
prop_insertOracleRoundTrip :: Property
prop_insertOracleRoundTrip = property $ do
  entries <- forAll genEntries
  key     <- forAll genMstKey
  val     <- forAll genCidBytes
  let expected = insertSorted key val entries
  case fromList entries of
    Nothing ->
      -- Empty tree: build from the single pair
      toList (singleton key val) === expected
    Just mst -> do
      let mst' = insert key val mst
      toList mst' === expected

-- | Inserting the same key twice with different values keeps only the last.
prop_insertIdempotent :: Property
prop_insertIdempotent = property $ do
  entries <- forAll genSomeEntries
  key     <- forAll genMstKey
  val1    <- forAll genCidBytes
  val2    <- forAll genCidBytes
  let mst' = insert key val2 (insert key val1 (fromNonEmpty entries))
  lookup key mst' === Just val2

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.MST.Build"
  [ ("encode/decode round-trip",                prop_encodeDecodeRoundTrip)
  , ("fromList empty input is Nothing",         prop_buildEmpty)
  , ("build then diff recovers entries",        prop_buildDiffRoundTrip)
  , ("build then lookup finds every key",       prop_buildGetRoundTrip)
  , ("build is deterministic",                  prop_buildDeterministic)
  , ("known single-entry vector",               prop_knownSingleEntry)
  , ("lookup missing key returns Nothing",      prop_buildGetMissing)
  , ("insert then lookup finds new value",      prop_insertLookup)
  , ("insert preserves other keys",             prop_insertPreservesOthers)
  , ("insert one-by-one matches fromList",      prop_insertMatchesFromList)
  , ("insert matches oracle toList",            prop_insertOracleRoundTrip)
  , ("double insert keeps last value",          prop_insertIdempotent)
  ]
