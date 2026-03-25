-- | MST construction from a sorted list of key\/value pairs.
--
-- Given a sorted list of @(key, valueCid)@ pairs this module builds a
-- complete Merkle Search Tree returning the root CID and a 'BlockMap'
-- of all generated node blocks.
--
-- The algorithm is a direct (batch) construction that mirrors the
-- upstream TypeScript implementation:
--
-- 1. Each key is assigned a /layer/ via 'leadingZerosOnHash'.
-- 2. Starting at the highest layer, keys at that layer become leaf
--    entries in the root node.
-- 3. Keys between consecutive same-layer entries are collected into
--    subtrees and built recursively at the next lower layer.
module ATProto.MST.Build
  ( buildMST
  ) where

import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import ATProto.Car.Cid      (CidBytes)
import ATProto.Car.BlockMap (BlockMap)
import ATProto.MST.Node     (NodeData (..), TreeEntry (..))
import ATProto.MST.Layer    (leadingZerosOnHash)
import ATProto.MST.Encode   (encodeNode, cidForDagCbor)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Build an MST from a sorted list of @(key, value CID)@ pairs.
--
-- The keys must be in ascending byte order with no duplicates.  Keys
-- should be in the form @\"collection\/rkey\"@.
--
-- Returns @('Nothing', empty)@ for an empty input, or
-- @('Just' rootCid, blocks)@ where @blocks@ contains all generated MST
-- node blocks keyed by their CIDs.
buildMST :: [(T.Text, CidBytes)] -> (Maybe CidBytes, BlockMap)
buildMST [] = (Nothing, Map.empty)
buildMST entries =
  let withLayers = [ (key, val, leadingZerosOnHash (TE.encodeUtf8 key))
                   | (key, val) <- entries
                   ]
      maxLayer   = maximum [ l | (_, _, l) <- withLayers ]
  in buildAtLayer withLayers maxLayer

-- ---------------------------------------------------------------------------
-- Recursive tree builder
-- ---------------------------------------------------------------------------

-- | Build a single MST node at the given @layer@.
--
-- All entries passed in must have their computed layer @<= layer@.
-- Entries whose layer equals @layer@ become leaf entries in this node;
-- the remaining entries (layer @< layer@) are grouped into left and
-- right subtrees built at @layer - 1@.
buildAtLayer
  :: [(T.Text, CidBytes, Int)]  -- ^ entries (key, value, layer)
  -> Int                         -- ^ current layer
  -> (Maybe CidBytes, BlockMap)
buildAtLayer [] _ = (Nothing, Map.empty)
buildAtLayer entries layer =
  let (leftGroup, groups) = splitByLayer entries layer

      -- Left subtree: all entries before the first entry at this layer.
      (mLeftCid, leftBlocks)
        | null leftGroup = (Nothing, Map.empty)
        | otherwise      = buildAtLayer leftGroup (layer - 1)

      -- Tree entries: one per same-layer key, each with an optional
      -- right subtree of the keys between it and the next same-layer key.
      (treeEntries, rightBlocks) = buildTreeEntries groups layer T.empty

      -- Assemble and serialise the node.
      node      = NodeData mLeftCid treeEntries
      nodeBytes = encodeNode node
      nodeCid   = cidForDagCbor nodeBytes

      allBlocks = Map.insert nodeCid nodeBytes
                    (Map.union leftBlocks rightBlocks)
  in (Just nodeCid, allBlocks)

-- ---------------------------------------------------------------------------
-- Entry grouping
-- ---------------------------------------------------------------------------

-- | An entry at the current layer together with the lower-layer entries
--   that sit to its right (before the next same-layer key).
type LayerGroup = (T.Text, CidBytes, [(T.Text, CidBytes, Int)])

-- | Split entries into the left group (entries before the first entry at
--   @layer@) and a list of 'LayerGroup's.
splitByLayer
  :: [(T.Text, CidBytes, Int)]
  -> Int
  -> ( [(T.Text, CidBytes, Int)]   -- left group
     , [LayerGroup]                 -- same-layer entries with right groups
     )
splitByLayer entries layer =
  let (left, rest) = span (\(_, _, l) -> l /= layer) entries
  in (left, groupByLayer rest layer)

-- | Given a list whose first element is at @layer@, partition into
--   consecutive 'LayerGroup's.
groupByLayer :: [(T.Text, CidBytes, Int)] -> Int -> [LayerGroup]
groupByLayer [] _ = []
groupByLayer ((key, val, _) : rest) layer =
  let (rightGroup, remaining) = span (\(_, _, l) -> l /= layer) rest
  in (key, val, rightGroup) : groupByLayer remaining layer

-- ---------------------------------------------------------------------------
-- Tree entry construction
-- ---------------------------------------------------------------------------

-- | Build 'TreeEntry's from 'LayerGroup's, threading the previous key
--   through for shared-prefix compression.
buildTreeEntries
  :: [LayerGroup]
  -> Int              -- ^ current layer
  -> T.Text           -- ^ previous full key
  -> ([TreeEntry], BlockMap)
buildTreeEntries [] _ _ = ([], Map.empty)
buildTreeEntries ((key, val, rightGroup) : rest) layer prevKey =
  let -- Right subtree for the entries between this key and the next.
      (mRightCid, rightBlocks)
        | null rightGroup = (Nothing, Map.empty)
        | otherwise       = buildAtLayer rightGroup (layer - 1)

      -- Shared-prefix compression against the previous key.
      prefixLen = countPrefixLen (TE.encodeUtf8 prevKey) (TE.encodeUtf8 key)
      suffix    = BS.drop prefixLen (TE.encodeUtf8 key)
      entry     = TreeEntry prefixLen suffix val mRightCid

      (restEntries, restBlocks) = buildTreeEntries rest layer key
  in (entry : restEntries, Map.union rightBlocks restBlocks)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Count the length of the common byte prefix of two 'BS.ByteString's.
countPrefixLen :: BS.ByteString -> BS.ByteString -> Int
countPrefixLen a b = go 0
  where
    len = min (BS.length a) (BS.length b)
    go i
      | i >= len              = i
      | BS.index a i == BS.index b i = go (i + 1)
      | otherwise             = i
