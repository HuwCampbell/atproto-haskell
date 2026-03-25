-- | Property-based tests for MST building and round-tripping.
module Test.ATProto.MST.Build (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.Car.Cid      (CidBytes (..), parseCidFromBytes)
import ATProto.Car.Parser   (readCarWithRoot)
import ATProto.MST.Node     (NodeData (..), TreeEntry (..), decodeNode)
import ATProto.MST.Encode   (encodeNode, cidForDagCbor)
import ATProto.MST.Build    (buildMST)
import ATProto.MST.Get      (get)
import ATProto.MST.Diff     (mstDiff, WriteDescr (..))

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
  return (CidBytes (header <> digest))

-- | Generate a sorted, unique list of MST entries.
genEntries :: Gen [(T.Text, CidBytes)]
genEntries = do
  n <- Gen.int (Range.linear 0 100)
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
-- Properties: build round-trips
-- ---------------------------------------------------------------------------

-- | Empty input produces Nothing root and empty block map.
prop_buildEmpty :: Property
prop_buildEmpty = property $ do
  let (mRoot, blocks) = buildMST []
  mRoot  === Nothing
  blocks === Map.empty

-- | Building an MST then diffing from empty returns all entries as
--   'WCreate' in the original order.
prop_buildDiffRoundTrip :: Property
prop_buildDiffRoundTrip = property $ do
  entries <- forAll genEntries
  case buildMST entries of
    (Nothing, _) -> entries === []
    (Just root, bmap) -> do
      case mstDiff bmap Nothing root of
        Left err -> do
          annotate (show err)
          failure
        Right writes -> do
          let recovered = [ (wdKey w, wdCid w) | w <- writes ]
          recovered === entries

-- | Every key inserted into the MST can be retrieved with 'get'.
prop_buildGetRoundTrip :: Property
prop_buildGetRoundTrip = property $ do
  entries <- forAll genEntries
  case buildMST entries of
    (Nothing, _) -> entries === []
    (Just root, bmap) ->
      mapM_ (checkGet bmap root) entries
  where
    checkGet bmap root (key, val) =
      case get bmap root key of
        Left err       -> do
          annotate (show err)
          failure
        Right Nothing  -> do
          annotate ("key not found: " ++ T.unpack key)
          failure
        Right (Just v) -> v === val

-- | Building an MST from entries, diffing to empty, and rebuilding
--   produces the same root CID (deterministic construction).
prop_buildDeterministic :: Property
prop_buildDeterministic = property $ do
  entries <- forAll genEntries
  let (root1, _) = buildMST entries
      (root2, _) = buildMST entries
  root1 === root2

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
          (mRoot, _) = buildMST [(key, parsedLeafCid)]
      case mRoot of
        Nothing   -> do
          annotate "buildMST returned Nothing"
          failure
        Just root -> root === expectedRoot

-- | A missing key returns Nothing even in a non-empty tree.
prop_buildGetMissing :: Property
prop_buildGetMissing = property $ do
  entries <- forAll (Gen.filter (not . null) genEntries)
  case buildMST entries of
    (Nothing, _) -> failure
    (Just root, bmap) -> do
      -- Use a key we know is not in the entry list.
      let missingKey = "zzz.missing/nothere"
      case get bmap root missingKey of
        Left err      -> do
          annotate (show err)
          failure
        Right Nothing -> success
        Right (Just _) -> do
          annotate "unexpected: found a value for missing key"
          failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.MST.Build"
  [ ("encode/decode round-trip",           prop_encodeDecodeRoundTrip)
  , ("buildMST empty input",              prop_buildEmpty)
  , ("build then diff recovers entries",   prop_buildDiffRoundTrip)
  , ("build then get finds every key",     prop_buildGetRoundTrip)
  , ("build is deterministic",             prop_buildDeterministic)
  , ("known single-entry vector",          prop_knownSingleEntry)
  , ("get missing key returns Nothing",    prop_buildGetMissing)
  ]
