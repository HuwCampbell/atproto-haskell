-- | Primary Merkle Search Tree data structure.
--
-- This module provides the core @MST@ type together with pure operations
-- for construction, traversal, lookup, diff, and proof verification.
--
-- The @BlockMap@ type is the serialisation format for MST trees, one can
-- 'fromBlockMap' / 'toBlockMap' to move between the two representations.
--
-- The biggest caveat for this module is that the parsing of tht MST is
-- strict and complete, so it can't work with partial modifications off of
-- the firehose.
--
-- The @BlockMap@ structure can store Merkle Search Tree nodes well as
-- actual record contents (think bluesky posts). The MST itself doesn't\
-- store any "real" data, just pointers to it based on the content hash.
module ATProto.MST.Tree
  ( -- * Types
    MST
  , mstCid
  , mstEntries

  , NodeEntry (..)
    -- * Write descriptor (for diff)
  , WriteDescr (..)
    -- * Zipper-based diff
  , DataDiff (..)
  , zipperDiff
    -- * Record operation (for proof verification)
  , RecordOp (..)
    -- * BlockMap serialisation
  , fromBlockMap
  , toBlockMap
    -- * Construction
  , singleton
  , fromNonEmpty
  , fromList
  , insert
    -- * Querying
  , rootCid
  , toList
  , lookup
  , member
    -- * Diff
  , diff
    -- * Proof verification
  , verifyProofs
    -- * Mutation
  , delete
    -- * Zipper
  , MSTZipper (..)
  , Crumb (..)
  , down
  , nextSibling
  , prevSibling
  , up
  , firstLeaf
  , nextLeaf
  ) where

import Prelude hiding (lookup)

import qualified Data.ByteString      as BS
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (maybeToList)
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import ATProto.Car.Cid      (CidBytes, cidToText)
import ATProto.Car.BlockMap (BlockMap)
import ATProto.MST.Node     (NodeData (..), TreeEntry (..), decodeNode)
import ATProto.MST.Encode   (encodeNode, cidForDagCbor)
import ATProto.MST.Layer    (leadingZerosOnHash)
import ATProto.MST.Types    (MstError (..))

-- ---------------------------------------------------------------------------
-- Core types
-- ---------------------------------------------------------------------------

-- | An in-memory Merkle Search Tree node with a memoised root CID.
--
-- The 'mstCid' is always computed bottom-up on construction via
-- 'fromBlockMap' or 'fromList' — it is a pure memo, never stored state.
data MST = MST
  { mstCid     :: CidBytes    -- ^ CID
  , mstEntries :: [NodeEntry] -- ^ ordered children
  } deriving (Eq, Show)

-- | An entry in an MST node.
data NodeEntry
  = SubTree MST               -- ^ an interior subtree
  | Leaf    T.Text CidBytes   -- ^ a leaf: fully-expanded key and value CID
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Write descriptor
-- ---------------------------------------------------------------------------

-- | A single write operation as seen in the diff between two MST states.
data WriteDescr
  = WCreate { wdKey :: T.Text, wdCid :: CidBytes }
    -- ^ A key present in the new tree but not in the old tree.
  | WUpdate { wdKey :: T.Text, wdPrev :: CidBytes, wdCid :: CidBytes }
    -- ^ A key present in both trees with different values.
  | WDelete { wdKey :: T.Text, wdPrev :: CidBytes }
    -- ^ A key present in the old tree but absent from the new tree.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Zipper-based diff result
-- ---------------------------------------------------------------------------

-- | Structured result of a zipper-based diff between two MST states.
--
-- Mirrors the TypeScript @DataDiff@ class in @\@bluesky-social/atproto@.
data DataDiff = DataDiff
  { ddAdds        :: Map.Map T.Text CidBytes
    -- ^ Leaves present in the new tree but absent from the old tree.
  , ddUpdates     :: Map.Map T.Text (CidBytes, CidBytes)
    -- ^ Leaves present in both trees with changed CIDs: key -> (old, new).
  , ddDeletes     :: Map.Map T.Text CidBytes
    -- ^ Leaves present in the old tree but absent from the new tree.
  , ddNewBlocks   :: BlockMap
    -- ^ New internal MST node blocks (present in new tree, absent from old).
  , ddNewLeafCids :: Set.Set CidBytes
    -- ^ Leaf value CIDs referenced by the new tree (records to preserve).
  , ddRemovedCids :: Set.Set CidBytes
    -- ^ Internal MST node CIDs present in the old tree but not in the new tree.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Record operation (for proof verification)
-- ---------------------------------------------------------------------------

-- | An assertion about a single record in the repository.
data RecordOp = RecordOp
  { ropKey       :: T.Text
    -- ^ The record key within the collection.
  , ropCid        :: Maybe CidBytes
    -- ^ Expected CID.  'Nothing' asserts the record is absent.
  }

-- ---------------------------------------------------------------------------
-- BlockMap serialisation
-- ---------------------------------------------------------------------------

-- | Deserialise an MST from a 'BlockMap', starting from the given root CID.
fromBlockMap :: BlockMap -> CidBytes -> Either MstError MST
fromBlockMap bmap cid = do
  raw      <- maybe (Left (MstNodeNotFound (cidToText cid)))
                    Right
                    (Map.lookup cid bmap)
  nodeData <- mapLeft (MstDecodeError . T.pack) (decodeNode raw)
  entries  <- loadEntries bmap "" (nodeEntries nodeData) (nodeLeft nodeData)
  return (MST cid entries)

-- | Load the flat 'NodeEntry' list from a decoded 'NodeData', recursively
-- loading subtrees from @bmap@.
loadEntries
  :: BlockMap
  -> T.Text          -- ^ previous fully-expanded key (for shared-prefix decoding)
  -> [TreeEntry]
  -> Maybe CidBytes  -- ^ nodeLeft
  -> Either MstError [NodeEntry]
loadEntries bmap prevKey entries mLeft = do
  leftEntries <- case mLeft of
    Nothing  -> Right []
    Just cid -> do sub <- fromBlockMap bmap cid
                   return [SubTree sub]
  entryNodes <- goEntries prevKey entries
  return (leftEntries <> entryNodes)
  where
    goEntries _ [] = Right []
    goEntries prev (te:rest) = do
      let prevBytes = TE.encodeUtf8 prev
          shared    = BS.take (tePrefix te) prevBytes
          fullKey   = TE.decodeUtf8 (shared <> teSuffix te)
      rightEntries <- case teRightTree te of
        Nothing  -> Right []
        Just cid -> do sub <- fromBlockMap bmap cid
                       return [SubTree sub]
      let leaf = Leaf fullKey (teValue te)
      restNodes <- goEntries fullKey rest
      return (leaf : rightEntries <> restNodes)

-- | Serialise an MST to a 'BlockMap' by folding over the tree, collecting
-- @(cid, encodedNodeBytes)@ pairs.
toBlockMap :: MST -> BlockMap
toBlockMap mst = go mst Map.empty
  where
    go :: MST -> BlockMap -> BlockMap
    go (MST cid entries) acc =
      let acc'  = foldl' processEntry acc entries
          nd    = toNodeData entries
          bytes = encodeNode nd
      in Map.insert cid bytes acc'

    processEntry acc (SubTree sub) = go sub acc
    processEntry acc (Leaf _ _)    = acc

-- | Convert a flat 'NodeEntry' list back to a 'NodeData' with
-- shared-prefix key compression, ready for CBOR encoding.
toNodeData :: [NodeEntry] -> NodeData
toNodeData entries =
  let (mLeft, rest) = case entries of
        (SubTree sub : r) -> (Just (mstCid sub), r)
        r                 -> (Nothing, r)
  in NodeData mLeft (buildTreeEntries rest T.empty)

-- | Build a list of 'TreeEntry' values from the @[NodeEntry]@ tail
-- (everything after the optional leading left subtree).
buildTreeEntries :: [NodeEntry] -> T.Text -> [TreeEntry]
buildTreeEntries [] _ = []
buildTreeEntries (Leaf key val : rest) prevKey =
  let prevBytes = TE.encodeUtf8 prevKey
      keyBytes  = TE.encodeUtf8 key
      prefixLen = countPrefixLen prevBytes keyBytes
      suffix    = BS.drop prefixLen keyBytes
      (mRight, rest') = case rest of
        (SubTree sub : r) -> (Just (mstCid sub), r)
        r                 -> (Nothing, r)
      te = TreeEntry prefixLen suffix val mRight
  in te : buildTreeEntries rest' key
buildTreeEntries (SubTree _ : rest) prevKey =
  -- A leading SubTree here would be a malformed tree; skip it.
  buildTreeEntries rest prevKey

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Build an MST containing exactly one key-value pair.
singleton :: T.Text -> CidBytes -> MST
singleton key val =
  let layer = leadingZerosOnHash (TE.encodeUtf8 key)
  in buildAtLayer (NE.singleton (key, val, layer)) layer

-- | Build an MST from a sorted list of @(key, value CID)@ pairs.
--
-- The keys must be in ascending byte order with no duplicates.  Returns
-- 'Nothing' for an empty input list.
fromList :: [(T.Text, CidBytes)] -> Maybe MST
fromList = fmap fromNonEmpty . NE.nonEmpty

-- | Build an MST from a sorted non-empty list of @(key, value CID)@ pairs.
--
-- The keys must be in ascending byte order with no duplicates.
fromNonEmpty :: NE.NonEmpty (T.Text, CidBytes) -> MST
fromNonEmpty entries =
  let withLayers = (\(key, val) -> (key, val, leadingZerosOnHash (TE.encodeUtf8 key))) <$> entries
      maxLayer   = maximum ((\(_, _, l) -> l) <$> withLayers)
  in buildAtLayer withLayers maxLayer

-- | Build a single MST node at the given layer.
buildAtLayer :: NE.NonEmpty (T.Text, CidBytes, Int) -> Int -> MST
buildAtLayer entries layer =
  let (leftGroup, groups) = splitByLayer entries layer
      mLeft               = (\ls -> buildAtLayer ls (layer - 1)) <$> NE.nonEmpty leftGroup
      nodeEntries_        = buildNodeEntries mLeft groups layer
      nd                  = toNodeData nodeEntries_
      bytes               = encodeNode nd
      nodeCid             = cidForDagCbor bytes
  in MST nodeCid nodeEntries_

-- | Build the flat 'NodeEntry' list for a node at @layer@.
buildNodeEntries :: Maybe MST -> [LayerGroup] -> Int -> [NodeEntry]
buildNodeEntries mLeft groups layer =
  let leftEntry   = maybe [] (\sub -> [SubTree sub]) mLeft
      rightGroups = concatMap buildGroup groups
  in leftEntry <> rightGroups
  where
    buildGroup (key, val, rightGroup) =
      let mRight     = (\rs -> buildAtLayer rs (layer - 1)) <$> NE.nonEmpty rightGroup
          rightEntry = maybe [] (\sub -> [SubTree sub]) mRight
      in Leaf key val : rightEntry

-- ---------------------------------------------------------------------------
-- Entry grouping (batch construction helpers)
-- ---------------------------------------------------------------------------

-- | An entry at the current layer together with the lower-layer entries
-- that sit to its right (before the next same-layer key).
type LayerGroup = (T.Text, CidBytes, [(T.Text, CidBytes, Int)])

-- | Split entries into the left group (entries before the first entry at
-- @layer@) and a list of 'LayerGroup's.
splitByLayer
  :: NE.NonEmpty (T.Text, CidBytes, Int)
  -> Int
  -> ( [(T.Text, CidBytes, Int)]  -- left group
     , [LayerGroup]                -- same-layer entries with right groups
     )
splitByLayer entries layer =
  let (left, rest) = NE.span (\(_, _, l) -> l /= layer) entries
  in (left, groupByLayer rest layer)

-- | Given a list whose first element is at @layer@, partition into
-- consecutive 'LayerGroup's.
groupByLayer :: [(T.Text, CidBytes, Int)] -> Int -> [LayerGroup]
groupByLayer [] _ = []
groupByLayer ((key, val, _) : rest) layer =
  let (rightGroup, remaining) = span (\(_, _, l) -> l /= layer) rest
  in (key, val, rightGroup) : groupByLayer remaining layer

-- ---------------------------------------------------------------------------
-- Querying
-- ---------------------------------------------------------------------------

-- | Return the root CID of the MST (the memoised hash of the root node).
-- Equivalent to the 'mstCid' record field.
rootCid :: MST -> CidBytes
rootCid = mstCid

-- | In-order traversal collecting all leaves as @(key, value CID)@ pairs.
toList :: MST -> [(T.Text, CidBytes)]
toList (MST _ entries) = concatMap entryLeaves entries
  where
    entryLeaves (Leaf k v)    = [(k, v)]
    entryLeaves (SubTree sub) = toList sub

-- | Look up the CID associated with a key.  Returns 'Nothing' if the key
-- is absent.  Pure — no 'Either', no blockstore.
lookup :: T.Text -> MST -> Maybe CidBytes
lookup key (MST _ entries) = search entries
  where
    search [] = Nothing
    search (Leaf k v : rest)
      | k == key  = Just v
      | k >  key  = Nothing   -- past the insertion point
      | otherwise = search rest
    search (SubTree sub : rest) =
      case firstLeafKey rest of
        Just nk | key >= nk -> search rest   -- key is to the right of subtree
        _                   -> lookup key sub -- descend into subtree

-- | Return the key of the first leaf reachable from the given entry list.
firstLeafKey :: [NodeEntry] -> Maybe T.Text
firstLeafKey []                = Nothing
firstLeafKey (Leaf k _ : _)   = Just k
firstLeafKey (SubTree _ : rest) = firstLeafKey rest

-- | Test whether a key is present in the tree.
member :: T.Text -> MST -> Bool
member key mst = maybe False (const True) (lookup key mst)

-- ---------------------------------------------------------------------------
-- Diff
-- ---------------------------------------------------------------------------

-- | Compute the list of writes that transform the old MST into the new MST.
--
-- @mOld = Nothing@ represents an empty tree (all entries in @new@ are
-- creates).
diff :: Maybe MST -> MST -> [WriteDescr]
diff mOld new = computeDiff (maybe [] toList mOld) (toList new)

-- | Given two sorted leaf lists, compute the set of writes.
computeDiff
  :: [(T.Text, CidBytes)]
  -> [(T.Text, CidBytes)]
  -> [WriteDescr]
computeDiff [] news = map (uncurry WCreate) news
computeDiff olds [] = map (uncurry WDelete) olds
computeDiff olds@((ok, ov):ot) news@((nk, nv):nt)
  | ok == nk && ov == nv = computeDiff ot nt
  | ok == nk             = WUpdate ok ov nv : computeDiff ot nt
  | ok <  nk             = WDelete ok ov    : computeDiff ot news
  | otherwise            = WCreate nk nv    : computeDiff olds nt

-- | Compute a structured diff between two MST states using the zipper
-- interface, co-iterating leaves in sorted order without flattening.
--
-- 'Nothing' for the old tree means an empty tree: all leaves in @new@ are
-- treated as additions.
--
-- Block tracking is done lazily: unchanged subtrees (identified by equal
-- CIDs) are skipped entirely, and node bytes are serialised only for MST
-- nodes that are genuinely new.
--
-- The leaf co-iteration is a single tail-recursive pass over a 'DataDiff'
-- accumulator.  The block-level fields ('ddNewBlocks', 'ddRemovedCids') are
-- pre-computed and placed in the initial accumulator, so the worker 'go' is
-- the single function that fills every field.
zipperDiff :: Maybe MST -> MST -> DataDiff
zipperDiff mOld new =
  let oldCids  = maybe Set.empty mstAllCids mOld
      newCids  = mstAllCids new
      initDiff = DataDiff
        { ddAdds        = Map.empty
        , ddUpdates     = Map.empty
        , ddDeletes     = Map.empty
        , ddNewBlocks   = mstNewBlocksNotIn oldCids new
        , ddNewLeafCids = Set.empty
        , ddRemovedCids = maybe Set.empty (mstRemovedCidsNotIn newCids) mOld
        }
      oldStart = mOld >>= \old -> firstLeaf (MSTZipper (SubTree old) [])
      newStart = firstLeaf (MSTZipper (SubTree new) [])
  in go oldStart newStart initDiff
  where
    go :: Maybe MSTZipper -> Maybe MSTZipper -> DataDiff -> DataDiff

    -- Both exhausted: done.
    go Nothing Nothing acc = acc

    -- Old exhausted: all remaining new leaves are additions.
    go Nothing (Just nz) acc =
      case zFocus nz of
        Leaf nk nv ->
          go Nothing (nextLeaf nz) acc
            { ddAdds        = Map.insert nk nv (ddAdds acc)
            , ddNewLeafCids = Set.insert nv (ddNewLeafCids acc)
            }
        SubTree _ -> acc  -- unreachable after firstLeaf

    -- New exhausted: all remaining old leaves are deletions.
    go (Just oz) Nothing acc =
      case zFocus oz of
        Leaf ok ov ->
          go (nextLeaf oz) Nothing acc
            { ddDeletes = Map.insert ok ov (ddDeletes acc) }
        SubTree _ -> acc  -- unreachable after firstLeaf

    -- Both active: compare current leaves.
    go (Just oz) (Just nz) acc =
      case (zFocus oz, zFocus nz) of
        (Leaf ok ov, Leaf nk nv)
          | ok == nk && ov == nv ->
              -- Identical: no change; new leaf CID is still live.
              go (nextLeaf oz) (nextLeaf nz) acc
                { ddNewLeafCids = Set.insert nv (ddNewLeafCids acc) }
          | ok == nk ->
              -- Same key, different CID: update.
              go (nextLeaf oz) (nextLeaf nz) acc
                { ddUpdates     = Map.insert ok (ov, nv) (ddUpdates acc)
                , ddNewLeafCids = Set.insert nv (ddNewLeafCids acc)
                }
          | ok < nk ->
              -- Old key has no counterpart in new: deletion.
              go (nextLeaf oz) (Just nz) acc
                { ddDeletes = Map.insert ok ov (ddDeletes acc) }
          | otherwise ->
              -- New key has no counterpart in old: addition.
              go (Just oz) (nextLeaf nz) acc
                { ddAdds        = Map.insert nk nv (ddAdds acc)
                , ddNewLeafCids = Set.insert nv (ddNewLeafCids acc)
                }
        _ ->
          -- Unreachable: firstLeaf/nextLeaf always yield a Leaf focus.
          acc

-- | Collect all MST node CIDs in a tree without serialising any of them.
-- Entire subtrees whose root CID is known to be absent can be short-circuited
-- by callers; this helper always visits every node.
mstAllCids :: MST -> Set.Set CidBytes
mstAllCids (MST cid entries) =
  Set.insert cid (Set.unions [ mstAllCids s | SubTree s <- entries ])

-- | Collect all MST node blocks, serialising only nodes whose CID is
-- absent from @knownCids@.  Entire subtrees are skipped when their root
-- CID is already known (content-addressed immutability guarantees all
-- descendants share the same property).
mstNewBlocksNotIn :: Set.Set CidBytes -> MST -> BlockMap
mstNewBlocksNotIn knownCids (MST cid entries)
  | Set.member cid knownCids = Map.empty
  | otherwise =
      let bytes = encodeNode (toNodeData entries)
          subs  = Map.unions [ mstNewBlocksNotIn knownCids s | SubTree s <- entries ]
      in Map.insert cid bytes subs

-- | Collect all MST node CIDs that are absent from @presentCids@.
-- Entire subtrees are skipped when their root CID is still present.
mstRemovedCidsNotIn :: Set.Set CidBytes -> MST -> Set.Set CidBytes
mstRemovedCidsNotIn presentCids (MST cid entries)
  | Set.member cid presentCids = Set.empty
  | otherwise =
      Set.insert cid
        (Set.unions [ mstRemovedCidsNotIn presentCids s | SubTree s <- entries ])


-- ---------------------------------------------------------------------------
-- Proof verification
-- ---------------------------------------------------------------------------

-- | Verify that all 'RecordOp's are consistent with the MST.
--
-- Returns @Right ()@ if every assertion holds, or @Left err@ on the first
-- mismatch.
verifyProofs :: MST -> [RecordOp] -> Either MstError ()
verifyProofs mst = mapM_ checkOp
  where
    checkOp (RecordOp key expectedCid) = do
      let foundCid = lookup key mst
      case (foundCid, expectedCid) of
        (Nothing, Nothing)                -> Right ()
        (Just c,  Just e)   | c == e      -> Right ()
                            | otherwise   -> Left (MstDecodeError
            (T.pack ("CID mismatch for key " <> T.unpack key)))
        (Nothing, Just _)                 -> Left (MstDecodeError
            (T.pack ("expected key present but absent: " <> T.unpack key)))
        (Just _,  Nothing)                -> Left (MstDecodeError
            (T.pack ("expected key absent but present: " <> T.unpack key)))

-- ---------------------------------------------------------------------------
-- Mutation
-- ---------------------------------------------------------------------------

-- | Delete a key from the MST.
--
-- If @key@ is present it is removed and any adjacent subtrees that were
-- separated by that leaf are merged back together.  The returned tree has a
-- freshly-computed root CID.
--
-- If @key@ is absent the tree is returned unchanged.
delete :: T.Text -> MST -> MST
delete key (MST _ entries) =
  trimTop . makeMST $
    go entries

  where
    go [] = []

    -- Leaf without a subtree to the left.
    go (Leaf k v : rest)
      | k == key  = rest
      | k >  key  = Leaf k v : rest
      | otherwise = Leaf k v : go rest

    -- SubTree immediately before a leaf.
    -- This case will merge adjacent subtrees if a leaf
    -- is removed from the middle.
    go (SubTree l : Leaf k v : rest)
      | k == key  = case rest of
                      SubTree r : rest' ->
                        let merged = mergeSubtrees l r
                        in SubTree merged : rest'
                      _ -> SubTree l : rest

      | key < k   = let l' = go (mstEntries l) in
                    if null l' then
                      Leaf k v : rest
                    else
                      SubTree (makeMST l') : Leaf k v : rest

      | otherwise = SubTree l : Leaf k v : go rest

    -- Trailing subtree.
    go [SubTree (MST _ sub)] =
      let sub' = go sub
      in [SubTree (makeMST sub') | not (null sub')]

    -- Consecutive subtrees should not arise in a valid MST.
    -- Skip defensively.
    go (SubTree sub : rest) = SubTree sub : go rest

    -- Trim the topmost nodes if they only point to single subtree.
    trimTop (MST _ [SubTree next]) =
      trimTop next
    trimTop other = other


-- | Insert or replace a key-value pair in the MST.
--
-- If @key@ is already present its value is replaced; otherwise a new leaf is
-- created at the correct layer.  The returned tree has a freshly-computed
-- root CID.
--
-- The operation is O(log n) in tree height for simple insertions (key lands
-- in an empty slot), and at most O(m) when an existing subtree of size @m@
-- must be split to make room for a leaf at the current node's layer.
insert :: T.Text -> CidBytes -> MST -> MST
insert key val mst =
  let keyLayer = leadingZerosOnHash (TE.encodeUtf8 key)
  in insertAtLayer keyLayer key val mst

-- ---------------------------------------------------------------------------
-- Insert internals
-- ---------------------------------------------------------------------------

-- | Dispatch insertion based on key layer vs node layer.
insertAtLayer :: Int -> T.Text -> CidBytes -> MST -> MST
insertAtLayer keyLayer key val mst =
  let layer = nodeLayer mst
  in case compare keyLayer layer of
       GT -> -- key's layer is above the current root; add a spine node on top
             insertAtLayer keyLayer key val (makeMST [SubTree mst])
       EQ -> makeMST (insertLeafAtLevel key val (mstEntries mst))
       LT -> makeMST (descendInsert keyLayer key val layer (mstEntries mst))

-- | Insert a leaf at the current level.
--
-- Scans the entry list left-to-right to find the sorted position.  If the
-- position falls inside an existing subtree, that subtree is split at @key@
-- and the two halves flank the new leaf.
insertLeafAtLevel :: T.Text -> CidBytes -> [NodeEntry] -> [NodeEntry]
insertLeafAtLevel key val = go
  where
    newLeaf = Leaf key val

    go [] = [newLeaf]

    go (Leaf k v : rest)
      | key <  k  = newLeaf : Leaf k v : rest   -- insert before
      | key == k  = newLeaf : rest               -- replace
      | otherwise = Leaf k v : go rest           -- keep scanning

    go (sub@(SubTree mst) : rest) =
      case firstLeafKey rest of
        -- The next leaf key tells us whether the new key is to the right
        -- of this subtree; if so, skip past it.
        Just nk | key >= nk -> sub : go rest
        _ ->
          -- The new key belongs at or before the next leaf, so it may
          -- land inside this subtree.  Split the subtree at `key`.
          let (mLeft, mRight) = splitAtKey key mst
          in  mconcat [
                maybeToList (SubTree <$> mLeft)
              , [newLeaf]
              , maybeToList (SubTree <$> mRight)
              , rest
              ]

-- | Descend into the appropriate subtree, creating one when none exists.
--
-- Called when @keyLayer < layer@ (the new key belongs below the current node).
descendInsert :: Int -> T.Text -> CidBytes -> Int -> [NodeEntry] -> [NodeEntry]
descendInsert keyLayer key val layer = go
  where
    -- A fresh subtree at (layer-1) containing just the new key.
    newSub = insertSingleKey (layer - 1) key val keyLayer

    go [] = [SubTree newSub]

    go (Leaf k v : rest)
      | key < k   = SubTree newSub : Leaf k v : rest  -- insert subtree before
      | otherwise = Leaf k v : go rest                 -- keep scanning

    go (SubTree sub : rest) =
      case firstLeafKey rest of
        Just nk | key >= nk -> SubTree sub : go rest  -- key is to the right
        _ -> SubTree (insertAtLayer keyLayer key val sub) : rest  -- recurse

-- | Build a subtree rooted at @targetLayer@ containing exactly one leaf at
-- @keyLayer@, creating spine nodes in between as required.
insertSingleKey :: Int -> T.Text -> CidBytes -> Int -> MST
insertSingleKey targetLayer key val keyLayer
  | targetLayer == keyLayer = makeMST [Leaf key val]
  | targetLayer >  keyLayer = makeMST [SubTree (insertSingleKey (targetLayer - 1) key val keyLayer)]
  | otherwise               = makeMST [Leaf key val]  -- shouldn't arise in a valid tree

-- | Split an MST at @key@: return the subtree of entries strictly before
-- @key@ and the subtree of entries strictly after @key@.
--
-- Both returned subtrees are wrapped back to the same layer as the input MST
-- so that the invariant (subtrees in a node are exactly one layer below the
-- node) is preserved.
splitAtKey :: T.Text -> MST -> (Maybe MST, Maybe MST)
splitAtKey key mst =
  let targetLayer = nodeLayer mst
      (ls, rs)    = span (\(k, _) -> k < key) (toList mst)
      wrapUp      = fmap (wrapToLayer targetLayer) . fromList
  in (wrapUp ls, wrapUp rs)

-- | Wrap an MST in spine nodes (each containing only a left subtree) until
-- its root is at @targetLayer@.
wrapToLayer :: Int -> MST -> MST
wrapToLayer targetLayer mst
  | nodeLayer mst >= targetLayer = mst
  | otherwise                    = wrapToLayer targetLayer (makeMST [SubTree mst])

-- | Compute the layer of an MST node.
--
-- The layer of a node equals the leading-zero count of any of its leaf keys;
-- all leaves in a node are at the same layer.  For pure spine nodes (no
-- leaves at this level) the layer is derived recursively from the single
-- subtree child.
nodeLayer :: MST -> Int
nodeLayer (MST _ entries) = go entries
  where
    go []                    = 0   -- degenerate; shouldn't arise in practice
    go (Leaf k _ : _)        = leadingZerosOnHash (TE.encodeUtf8 k)
    go (SubTree sub : rest)  =
      case firstLeafKey rest of
        Just k  -> leadingZerosOnHash (TE.encodeUtf8 k)  -- a sibling leaf tells us the layer
        Nothing -> nodeLayer sub + 1                      -- spine node: one above the child

-- | Build an MST node from a flat entry list, computing the memoised CID.
makeMST :: [NodeEntry] -> MST
makeMST entries =
  let nd    = toNodeData entries
      bytes = encodeNode nd
      cid   = cidForDagCbor bytes
  in MST cid entries

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Count the length of the common byte prefix of two 'BS.ByteString's.
countPrefixLen :: BS.ByteString -> BS.ByteString -> Int
countPrefixLen a b = go 0
  where
    len = min (BS.length a) (BS.length b)
    go i
      | i >= len                      = i
      | BS.index a i == BS.index b i  = go (i + 1)
      | otherwise                     = i

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left e)  = Left (f e)
mapLeft _ (Right x) = Right x

-- ---------------------------------------------------------------------------
-- Zipper Implementation
-- ---------------------------------------------------------------------------

-- | A zipper focused on a position in the MST.
--
-- The zipper is useful for traversals that need O(1) sibling movement and
-- precise positional context — most notably computing diffs between two
-- trees by co-iterating their leaves in sorted order.
data MSTZipper = MSTZipper
  { zFocus       :: NodeEntry
  , zBreadcrumbs :: [Crumb]
  } deriving (Eq, Show)

-- | One level of breadcrumb context.
data Crumb = Crumb
  { crumbLefts  :: [NodeEntry]  -- ^ Entries to the left of the focus (reversed).
  , crumbRights :: [NodeEntry]  -- ^ Entries to the right of the focus.
  } deriving (Eq, Show)

-- | Descend into the @idx@-th entry (0-indexed) of the focused 'SubTree'.
--
-- Returns 'Nothing' if the focus is not a 'SubTree' or if @idx@ is out of
-- range.
down :: Int -> MSTZipper -> Maybe MSTZipper
down idx (MSTZipper (SubTree (MST _ entries)) crumbs) =
  case splitAt idx entries of
    (lefts, entry : rights) ->
      let newCrumb = Crumb (reverse lefts) rights
      in Just (MSTZipper entry (newCrumb : crumbs))
    _ -> Nothing
down _ _ = Nothing

-- | Move to the next sibling entry at the current level (O(1)).
nextSibling :: MSTZipper -> Maybe MSTZipper
nextSibling (MSTZipper focus (Crumb lefts (r:rights) : crumbs)) =
  let newCrumb = Crumb (focus : lefts) rights
  in Just (MSTZipper r (newCrumb : crumbs))
nextSibling _ = Nothing

-- | Move to the previous sibling entry at the current level (O(1)).
prevSibling :: MSTZipper -> Maybe MSTZipper
prevSibling (MSTZipper focus (Crumb (l:lefts) rights : crumbs)) =
  let newCrumb = Crumb lefts (focus : rights)
  in Just (MSTZipper l (newCrumb : crumbs))
prevSibling _ = Nothing

-- | Go back to the parent, recomputing the parent node's CID.
up :: MSTZipper -> Maybe MSTZipper
up (MSTZipper _ []) = Nothing
up (MSTZipper focus (Crumb lefts rights : crumbs)) =
  let newEntries = reverseOnto (focus : rights) lefts
      newMST     = makeMST newEntries
  in Just $ MSTZipper (SubTree newMST) crumbs

-- | @reverseOnto tail xs@ = @reverse xs <> tail@.
--
-- Used by 'up' to reconstruct the parent's entry list from the stored
-- (reversed) left context and the right context.
reverseOnto :: [a] -> [a] -> [a]
reverseOnto = foldl' (flip (:))

-- | Move the zipper to the first (leftmost) leaf reachable from the current
-- focus.
--
-- If the focus is already a 'Leaf', it is returned as-is.  If it is a
-- 'SubTree', the zipper descends into the first entry and recurses.
-- Returns 'Nothing' for an empty subtree.
firstLeaf :: MSTZipper -> Maybe MSTZipper
firstLeaf z@(MSTZipper (Leaf _ _) _)  = Just z
firstLeaf z@(MSTZipper (SubTree _) _) = down 0 z >>= firstLeaf

-- | Advance the zipper to the next leaf in in-order (sorted-key) traversal.
--
-- The algorithm is:
--
--  1. Move to the next sibling.
--  2. If the sibling is a 'Leaf', we are done.
--  3. If the sibling is a 'SubTree', descend to its first leaf.
--  4. If there is no next sibling, go up one level and repeat.
--  5. When the root is reached with no next sibling, the traversal is
--     exhausted.
nextLeaf :: MSTZipper -> Maybe MSTZipper
nextLeaf z = case nextSibling z of
  Just z' -> case zFocus z' of
    Leaf _ _  -> Just z'
    SubTree _ -> firstLeaf z'
  Nothing ->
    up z >>= nextLeaf

-- ---------------------------------------------------------------------------
-- Shared helper (used by both delete and the zipper)
-- ---------------------------------------------------------------------------

-- | Merge two MSTs (at the same layer) into one.
--
-- Combines all leaves from @l@ (all strictly less than those of @r@) with
-- those of @r@ and rebuilds a canonical MST at the same layer.
mergeSubtrees :: MST -> MST -> MST
mergeSubtrees (MST _ left) (MST _ right) =
  case (reverse left, right) of
    (SubTree l : ls, SubTree r : rs) ->
      let newMiddle = mergeSubtrees l r
      in makeMST (reverseOnto (SubTree newMiddle : rs) ls)
    (ls, rs) ->
      makeMST (reverseOnto rs ls)
