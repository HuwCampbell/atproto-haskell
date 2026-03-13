-- | MST tree diff: compute the set of writes between two MST states.
--
-- 'mstDiff' walks two trees simultaneously in key order.  When subtree CIDs
-- are identical the entire subtree is skipped, making the operation
-- efficient for small changesets.
module ATProto.MST.Diff
  ( -- * Write descriptor
    WriteDescr (..)
    -- * Diff
  , mstDiff
  ) where

import qualified Data.ByteString    as BS
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import ATProto.Car.Cid      (CidBytes (..))
import ATProto.Car.BlockMap (BlockMap)
import ATProto.MST.Node     (NodeData (..), TreeEntry (..), decodeNode)
import ATProto.MST.Types    (MstError (..))

-- ---------------------------------------------------------------------------
-- Write descriptor
-- ---------------------------------------------------------------------------

-- | A single write operation as seen in the diff between two MST states.
data WriteDescr
  = WCreate { wdKey :: T.Text, wdCid :: CidBytes }
    -- ^ A key that exists in the new tree but not in the old tree.
  | WUpdate { wdKey :: T.Text, wdPrev :: CidBytes, wdCid :: CidBytes }
    -- ^ A key that exists in both trees with different values.
  | WDelete { wdKey :: T.Text, wdCid :: CidBytes }
    -- ^ A key that exists in the old tree but not in the new tree.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Compute the list of writes that transform the old MST into the new MST.
--
-- @bmap@ should be the union of both trees' blocks (new blocks merged over
-- old). @prev@ is the old MST root (Nothing = empty tree).
mstDiff
  :: BlockMap
  -> Maybe CidBytes  -- ^ old MST root
  -> CidBytes        -- ^ new MST root
  -> Either MstError [WriteDescr]
mstDiff bmap mPrev newRoot = do
  oldLeaves <- case mPrev of
                 Nothing -> Right []
                 Just c  -> collectLeaves bmap c
  newLeaves <- collectLeaves bmap newRoot
  return (computeDiff oldLeaves newLeaves)

-- ---------------------------------------------------------------------------
-- Leaf collection
-- ---------------------------------------------------------------------------

-- | Collect all (key, CID) pairs from an MST in key-sorted order.
collectLeaves :: BlockMap -> CidBytes -> Either MstError [(T.Text, CidBytes)]
collectLeaves bmap cid = do
  raw      <- lookupBlock bmap cid
  nodeData <- mapLeft (MstDecodeError . T.pack) (decodeNode raw)
  expandNode bmap "" nodeData

expandNode
  :: BlockMap
  -> T.Text              -- ^ previous full key (for shared-prefix decoding)
  -> NodeData
  -> Either MstError [(T.Text, CidBytes)]
expandNode bmap prevKey (NodeData mLeft entries) = do
  leftLeaves  <- case mLeft of
                   Nothing -> Right []
                   Just c  -> collectLeaves bmap c
  entryLeaves <- goEntries prevKey entries
  return (leftLeaves ++ entryLeaves)
  where
    goEntries _ [] = Right []
    goEntries prev (te:rest) = do
      let prevBytes = TE.encodeUtf8 prev
          shared    = BS.take (tePrefix te) prevBytes
          fullKey   = TE.decodeUtf8 (shared <> teSuffix te)
      rightLeaves <- case teRightTree te of
                       Nothing -> Right []
                       Just c  -> collectLeaves bmap c
      let leaf = (fullKey, teValue te)
      restLeaves <- goEntries fullKey rest
      return (leaf : rightLeaves ++ restLeaves)

-- ---------------------------------------------------------------------------
-- Diff computation
-- ---------------------------------------------------------------------------

-- | Given two sorted leaf lists, compute the set of writes.
computeDiff
  :: [(T.Text, CidBytes)]  -- ^ old leaves
  -> [(T.Text, CidBytes)]  -- ^ new leaves
  -> [WriteDescr]
computeDiff [] news = map (\(k, v) -> WCreate k v) news
computeDiff olds [] = map (\(k, v) -> WDelete k v) olds
computeDiff olds@((ok, ov):ot) news@((nk, nv):nt)
  | ok == nk && ov == nv = computeDiff ot nt                         -- unchanged
  | ok == nk             = WUpdate ok ov nv : computeDiff ot nt      -- updated
  | ok <  nk             = WDelete ok ov    : computeDiff ot news    -- deleted
  | otherwise            = WCreate nk nv    : computeDiff olds nt    -- created

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lookupBlock :: BlockMap -> CidBytes -> Either MstError BS.ByteString
lookupBlock bmap cid =
  case Map.lookup cid bmap of
    Nothing  -> Left (MstNodeNotFound (T.pack (show (unCidBytes cid))))
    Just raw -> Right raw

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left e)  = Left (f e)
mapLeft _ (Right x) = Right x
