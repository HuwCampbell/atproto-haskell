-- | MST point lookup.
--
-- This module is a thin wrapper over "ATProto.MST.Tree".  It retains the
-- original @BlockMap@-based signature for backward compatibility.
module ATProto.MST.Get
  ( get
  ) where

import qualified Data.Text as T

import ATProto.Car.Cid      (CidBytes)
import ATProto.Car.BlockMap (BlockMap)
import ATProto.MST.Tree     (fromBlockMap, lookup)
import ATProto.MST.Types    (MstError)

import Prelude hiding (lookup)

-- | Look up a key in the MST rooted at @rootCid@.
--
-- Returns @Right (Just cid)@ if the key is present, @Right Nothing@ if
-- absent, or @Left err@ if a node could not be decoded.
get :: BlockMap -> CidBytes -> T.Text -> Either MstError (Maybe CidBytes)
get bmap cid key = do
  mst <- fromBlockMap bmap cid
  return (lookup key mst)
