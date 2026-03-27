-- | MST proof verification.
--
-- This module is a thin wrapper over "ATProto.MST.Tree".  It retains the
-- original @BlockMap@-based signature for backward compatibility.
module ATProto.MST.Verify
  ( -- * Record operation
    RecordOp (..)
    -- * Verification
  , verifyProofs
  ) where

import ATProto.Car.Cid      (CidBytes)
import ATProto.Car.BlockMap (BlockMap)
import ATProto.MST.Tree     (RecordOp (..))
import ATProto.MST.Types    (MstError)
import qualified ATProto.MST.Tree as Tree

-- | Verify that all 'RecordOp's are consistent with the MST.
--
-- Returns @Right ()@ if every assertion holds, or @Left err@ on the first
-- mismatch or lookup error.
verifyProofs
  :: BlockMap
  -> CidBytes     -- ^ MST root CID
  -> [RecordOp]
  -> Either MstError ()
verifyProofs bmap cid ops = do
  mst <- Tree.fromBlockMap bmap cid
  Tree.verifyProofs mst ops
