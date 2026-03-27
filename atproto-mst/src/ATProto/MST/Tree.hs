-- | Primary in-memory MST data structure.
--
-- This module provides the core @MST@ type together with pure operations
-- for construction, traversal, lookup, diff, and proof verification.
-- The @BlockMap@ type is demoted to a serialisation format: use
-- 'fromBlockMap' / 'toBlockMap' to move between the two representations.
--
-- Desired layering:
--
-- > [BlockMap / CAR file] <-> fromBlockMap / toBlockMap <-> [MST] <- lookup / toList / diff
module ATProto.MST.Tree
  ( -- * Types
    MST (..)
  , NodeEntry (..)
    -- * Write descriptor (for diff)
  , WriteDescr (..)
    -- * Record operation (for proof verification)
  , RecordOp (..)
    -- * BlockMap serialisation
  , fromBlockMap
  , toBlockMap
    -- * Construction
  , fromList
    -- * Querying
  , rootCid
  , toList
  , lookup
  , member
    -- * Diff
  , diff
    -- * Proof verification
  , verifyProofs
  ) where

import Prelude hiding (lookup)

import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import ATProto.Car.Cid      (CidBytes (..))
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
  { mstCid     :: CidBytes    -- ^ memoised CID (hash of this node's CBOR)
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
  | WDelete { wdKey :: T.Text, wdCid :: CidBytes }
    -- ^ A key present in the old tree but absent from the new tree.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Record operation (for proof verification)
-- ---------------------------------------------------------------------------

-- | An assertion about a single record in the repository.
data RecordOp = RecordOp
  { ropCollection :: T.Text
    -- ^ The Lexicon collection NSID, e.g. @\"app.bsky.feed.post\"@.
  , ropRkey       :: T.Text
    -- ^ The record key within the collection.
  , ropCid        :: Maybe CidBytes
    -- ^ Expected CID.  'Nothing' asserts the record is absent.
  }

-- ---------------------------------------------------------------------------
-- BlockMap serialisation
-- ---------------------------------------------------------------------------

-- | Deserialise an MST from a 'BlockMap', starting from the given root CID.
--
-- Each subtree CID in the loaded node triggers a recursive call; GHC's lazy
-- evaluation means sub-calls are thunks until the subtree is actually
-- traversed.
fromBlockMap :: BlockMap -> CidBytes -> Either MstError MST
fromBlockMap bmap cid = do
  raw      <- maybe (Left (MstNodeNotFound (T.pack (show (unCidBytes cid)))))
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
  return (leftEntries ++ entryNodes)
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
      return (leaf : rightEntries ++ restNodes)

-- | Serialise an MST to a 'BlockMap' by folding over the tree, collecting
-- @(cid, encodedNodeBytes)@ pairs.
toBlockMap :: MST -> BlockMap
toBlockMap mst = go mst Map.empty
  where
    go :: MST -> BlockMap -> BlockMap
    go (MST cid entries) acc =
      let acc'  = foldl processEntry acc entries
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

-- | Build an MST from a sorted list of @(key, value CID)@ pairs.
--
-- The keys must be in ascending byte order with no duplicates.  Returns
-- 'Nothing' for an empty input list.
fromList :: [(T.Text, CidBytes)] -> Maybe MST
fromList [] = Nothing
fromList entries =
  let withLayers = [ (key, val, leadingZerosOnHash (TE.encodeUtf8 key))
                   | (key, val) <- entries ]
      maxLayer   = maximum [ l | (_, _, l) <- withLayers ]
  in buildAtLayer withLayers maxLayer

-- | Build a single MST node at the given layer.
buildAtLayer :: [(T.Text, CidBytes, Int)] -> Int -> Maybe MST
buildAtLayer [] _ = Nothing
buildAtLayer entries layer =
  let (leftGroup, groups) = splitByLayer entries layer
      mLeft               = buildAtLayer leftGroup (layer - 1)
      nodeEntries_        = buildNodeEntries mLeft groups layer
      nd                  = toNodeData nodeEntries_
      bytes               = encodeNode nd
      nodeCid             = cidForDagCbor bytes
  in Just (MST nodeCid nodeEntries_)

-- | Build the flat 'NodeEntry' list for a node at @layer@.
buildNodeEntries :: Maybe MST -> [LayerGroup] -> Int -> [NodeEntry]
buildNodeEntries mLeft groups layer =
  let leftEntry   = maybe [] (\sub -> [SubTree sub]) mLeft
      rightGroups = concatMap buildGroup groups
  in leftEntry ++ rightGroups
  where
    buildGroup (key, val, rightGroup) =
      let mRight     = buildAtLayer rightGroup (layer - 1)
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
  :: [(T.Text, CidBytes, Int)]
  -> Int
  -> ( [(T.Text, CidBytes, Int)]  -- left group
     , [LayerGroup]                -- same-layer entries with right groups
     )
splitByLayer entries layer =
  let (left, rest) = span (\(_, _, l) -> l /= layer) entries
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
computeDiff [] news = map (\(k, v) -> WCreate k v) news
computeDiff olds [] = map (\(k, v) -> WDelete k v) olds
computeDiff olds@((ok, ov):ot) news@((nk, nv):nt)
  | ok == nk && ov == nv = computeDiff ot nt
  | ok == nk             = WUpdate ok ov nv : computeDiff ot nt
  | ok <  nk             = WDelete ok ov    : computeDiff ot news
  | otherwise            = WCreate nk nv    : computeDiff olds nt

-- ---------------------------------------------------------------------------
-- Proof verification
-- ---------------------------------------------------------------------------

-- | Verify that all 'RecordOp's are consistent with the MST.
--
-- Returns @Right ()@ if every assertion holds, or @Left err@ on the first
-- mismatch.
verifyProofs :: MST -> [RecordOp] -> Either MstError ()
verifyProofs mst ops = mapM_ checkOp ops
  where
    checkOp (RecordOp col rkey expectedCid) = do
      let key = col <> "/" <> rkey
      let foundCid = lookup key mst
      case (foundCid, expectedCid) of
        (Nothing, Nothing)                -> Right ()
        (Just c,  Just e)   | c == e      -> Right ()
                            | otherwise   -> Left (MstDecodeError
            (T.pack ("CID mismatch for key " ++ T.unpack key)))
        (Nothing, Just _)                 -> Left (MstDecodeError
            (T.pack ("expected key present but absent: " ++ T.unpack key)))
        (Just _,  Nothing)                -> Left (MstDecodeError
            (T.pack ("expected key absent but present: " ++ T.unpack key)))

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
