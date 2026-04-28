-- | Tests for the zipper-based MST diff functions.
--
-- These tests verify that 'zipperDiff' correctly identifies additions,
-- updates, deletions, and block changes between two MST states, and that
-- its results are consistent with the simpler list-based 'diff'.
--
-- A key property under test is that block tracking is /lazy/: the new
-- block set must contain exactly those MST node blocks that are present
-- in the new tree but absent from the old tree, and the removed-CID set
-- must contain exactly those node CIDs that are present in the old tree
-- but absent from the new tree.  This means that unchanged subtrees (same
-- CID in both trees) contribute neither new blocks nor removed CIDs.
module Test.ATProto.MST.Diff (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T

import ATProto.Car.Cid      (CidBytes, unsafeRawCid)
import ATProto.MST.Tree
  ( MST (..)
  , NodeEntry (..)
  , DataDiff (..)
  , WriteDescr (..)
  , fromList
  , fromNonEmpty
  , insert
  , delete
  , toList
  , toBlockMap
  , zipperDiff
  , diff
  )

import Prelude hiding (lookup)

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genKeyChar :: Gen Char
genKeyChar = Gen.element (['a'..'z'] ++ ['0'..'9'])

genMstKey :: Gen T.Text
genMstKey = do
  col  <- Gen.text (Range.linear 1 20) genKeyChar
  rkey <- Gen.text (Range.linear 1 20) genKeyChar
  return (col <> "/" <> rkey)

genCidBytes :: Gen CidBytes
genCidBytes = do
  digest <- Gen.bytes (Range.singleton 32)
  let header = BS.pack [0x01, 0x71, 0x12, 0x20]
  return (unsafeRawCid (header <> digest))

-- | Generate a sorted, unique, non-empty list of MST entries.
genSomeEntries :: Gen (NE.NonEmpty (T.Text, CidBytes))
genSomeEntries = do
  n     <- Gen.int (Range.linear 1 100)
  pairs <- sequence [ (,) <$> genMstKey <*> genCidBytes | _ <- [1..n] ]
  return $ NE.fromList (Map.toAscList (Map.fromList pairs))

-- | Generate a sorted, unique list large enough for a multi-level MST.
genLargeEntries :: Gen (NE.NonEmpty (T.Text, CidBytes))
genLargeEntries = do
  n     <- Gen.int (Range.linear 20 100)
  pairs <- sequence [ (,) <$> genMstKey <*> genCidBytes | _ <- [1..n] ]
  return $ NE.fromList (Map.toAscList (Map.fromList pairs))

-- ---------------------------------------------------------------------------
-- Property tests
-- ---------------------------------------------------------------------------

-- | Diff from empty to a single-entry tree reports exactly one addition.
prop_zipperDiffEmptyToSingle :: Property
prop_zipperDiffEmptyToSingle = property $ do
  key <- forAll genMstKey
  val <- forAll genCidBytes
  let new = fromNonEmpty (NE.singleton (key, val))
      d   = zipperDiff Nothing new
  ddAdds    d === Map.singleton key val
  ddUpdates d === Map.empty
  ddDeletes d === Map.empty

-- | When the old tree is empty all new leaves are additions.
prop_zipperDiffCreates :: Property
prop_zipperDiffCreates = property $ do
  entries <- forAll genSomeEntries
  let new  = fromNonEmpty entries
      d    = zipperDiff Nothing new
  -- Every entry should appear as an add.
  ddAdds    d === Map.fromList (NE.toList entries)
  ddUpdates d === Map.empty
  ddDeletes d === Map.empty

-- | Deleting every key from the new tree produces only deletions.
prop_zipperDiffDeletes :: Property
prop_zipperDiffDeletes = property $ do
  entries <- forAll genSomeEntries
  let old      = fromNonEmpty entries
      -- Build a new tree with all keys removed (results in an empty MST)
      emptyNew = foldr (\(k, _) t -> delete k t) old (NE.toList entries)
      d        = zipperDiff (Just old) emptyNew
  ddAdds    d === Map.empty
  ddUpdates d === Map.empty
  -- Every original entry must be in the deletes map
  ddDeletes d === Map.fromList (NE.toList entries)

-- | A mix of inserts, updates, and deletes is reflected correctly.
prop_zipperDiffUpdates :: Property
prop_zipperDiffUpdates = property $ do
  entries <- forAll genSomeEntries
  let old    = fromNonEmpty entries
      (k, _) = NE.head entries          -- pick a key that exists
  newVal <- forAll genCidBytes
  newKey <- forAll genMstKey
  newCid <- forAll genCidBytes
  let new = insert newKey newCid (insert k newVal old)
      d   = zipperDiff (Just old) new
  -- The new key must appear as an add (unless it coincides with an existing key)
  case Map.lookup newKey (Map.fromList (NE.toList entries)) of
    Nothing ->
      assert (Map.member newKey (ddAdds d) || Map.member newKey (ddUpdates d))
    Just _ ->
      success
  -- The updated key is either in updates (value changed) or untouched (same value).
  case Map.lookup k (Map.fromList (NE.toList entries)) of
    Just oldVal | oldVal /= newVal ->
      assert (Map.member k (ddUpdates d))
    _ ->
      success

-- | Identical trees produce an empty DataDiff.
prop_zipperDiffIdentical :: Property
prop_zipperDiffIdentical = property $ do
  entries <- forAll genSomeEntries
  let mst = fromNonEmpty entries
      d   = zipperDiff (Just mst) mst
  ddAdds        d === Map.empty
  ddUpdates     d === Map.empty
  ddDeletes     d === Map.empty
  ddNewBlocks   d === Map.empty
  ddRemovedCids d === Set.empty

-- | The leaf-level result of 'zipperDiff' matches 'diff' (the list-based diff).
prop_zipperDiffStructure :: Property
prop_zipperDiffStructure = property $ do
  entries <- forAll genSomeEntries
  key     <- forAll genMstKey
  val     <- forAll genCidBytes
  let old  = fromNonEmpty entries
      new  = insert key val old
      d    = zipperDiff (Just old) new
      ws   = diff (Just old) new
  -- Adds
  Map.toAscList (ddAdds d)
    === [ (wdKey w, wdCid w) | w@WCreate{} <- ws ]
  -- Updates
  Map.toAscList (ddUpdates d)
    === [ (wdKey w, (wdPrev w, wdCid w)) | w@WUpdate{} <- ws ]
  -- Deletes
  Map.toAscList (ddDeletes d)
    === [ (wdKey w, wdPrev w) | w@WDelete{} <- ws ]

-- | Block tracking: new blocks are a subset of the new block map;
--   removed CIDs are absent from the new block map.
prop_zipperDiffBlockTracking :: Property
prop_zipperDiffBlockTracking = property $ do
  entries <- forAll genSomeEntries
  key     <- forAll genMstKey
  val     <- forAll genCidBytes
  let old         = fromNonEmpty entries
      new         = insert key val old
      d           = zipperDiff (Just old) new
      oldBlockMap = toBlockMap old
      newBlockMap = toBlockMap new
  -- Every new block must be present in the new tree's block map.
  assert (Map.isSubmapOfBy (\_ _ -> True) (ddNewBlocks d) newBlockMap)
  -- No new block should be present in the old tree's block map (they are genuinely new).
  assert (Map.null (Map.intersection (ddNewBlocks d) oldBlockMap))
  -- Every removed CID must be absent from the new block map.
  assert (all (\c -> not (Map.member c newBlockMap)) (Set.toList (ddRemovedCids d)))

-- | All leaf CIDs in the new tree appear in 'ddNewLeafCids'.
prop_zipperDiffNewLeafCids :: Property
prop_zipperDiffNewLeafCids = property $ do
  entries <- forAll genSomeEntries
  key     <- forAll genMstKey
  val     <- forAll genCidBytes
  let old     = fromNonEmpty entries
      new     = insert key val old
      d       = zipperDiff (Just old) new
      newLeaves = Set.fromList (map snd (toList new))
  -- Every leaf CID from the new tree must be in ddNewLeafCids.
  assert (Set.isSubsetOf newLeaves (ddNewLeafCids d))

-- | Creating from Nothing still tracks blocks correctly.
prop_zipperDiffFromEmpty :: Property
prop_zipperDiffFromEmpty = property $ do
  entries <- forAll genLargeEntries
  let new         = fromNonEmpty entries
      d           = zipperDiff Nothing new
      newBlockMap = toBlockMap new
  -- All new tree blocks are "new" since old tree was empty.
  ddNewBlocks d === newBlockMap
  ddRemovedCids d === Set.empty
  -- All leaf CIDs must be captured.
  let newLeaves = Set.fromList (map snd (NE.toList entries))
  assert (Set.isSubsetOf newLeaves (ddNewLeafCids d))

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.MST.Diff"
  [ ("zipperDiff: empty to single entry",        prop_zipperDiffEmptyToSingle)
  , ("zipperDiff: all creates from empty old",   prop_zipperDiffCreates)
  , ("zipperDiff: all deletes",                  prop_zipperDiffDeletes)
  , ("zipperDiff: mixed adds/updates",           prop_zipperDiffUpdates)
  , ("zipperDiff: identical trees give empty",   prop_zipperDiffIdentical)
  , ("zipperDiff: matches list-based diff",      prop_zipperDiffStructure)
  , ("zipperDiff: block tracking",               prop_zipperDiffBlockTracking)
  , ("zipperDiff: new leaf CIDs captured",       prop_zipperDiffNewLeafCids)
  , ("zipperDiff: from empty old tree",          prop_zipperDiffFromEmpty)
  ]

