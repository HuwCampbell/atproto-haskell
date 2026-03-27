-- | MST tree diff: compute the set of writes between two MST states.
--
-- This module is a thin wrapper over "ATProto.MST.Tree".  It retains the
-- original @BlockMap@-based signature for backward compatibility.
module ATProto.MST.Diff
  ( -- * Write descriptor
    WriteDescr (..)
    -- * Diff
  , mstDiff
  ) where

import ATProto.Car.Cid      (CidBytes)
import ATProto.Car.BlockMap (BlockMap)
import ATProto.MST.Tree     (WriteDescr (..), fromBlockMap, diff)
import ATProto.MST.Types    (MstError)

-- | Compute the list of writes that transform the old MST into the new MST.
--
-- @bmap@ should be the union of both trees' blocks (new blocks merged over
-- old). @prev@ is the old MST root (Nothing = empty tree).
mstDiff
  :: BlockMap
  -> Maybe CidBytes  -- ^ old MST root
  -> CidBytes        -- ^ new MST root
  -> Either MstError [WriteDescr]
mstDiff bmap mOld newCid = do
  mOldMst <- traverse (fromBlockMap bmap) mOld
  newMst  <- fromBlockMap bmap newCid
  return (diff mOldMst newMst)
