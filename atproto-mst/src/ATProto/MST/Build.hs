-- | MST construction from a sorted list of key\/value pairs.
--
-- This module is a thin wrapper over "ATProto.MST.Tree".  It retains the
-- original @(Maybe CidBytes, BlockMap)@ return type for backward
-- compatibility with callers that have not yet migrated to the new API.
module ATProto.MST.Build
  ( buildMST
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

import ATProto.Car.Cid      (CidBytes)
import ATProto.Car.BlockMap (BlockMap)
import ATProto.MST.Tree     (fromList, rootCid, toBlockMap)

-- | Build an MST from a sorted list of @(key, value CID)@ pairs.
--
-- The keys must be in ascending byte order with no duplicates.  Keys
-- should be in the form @\"collection\/rkey\"@.
--
-- Returns @('Nothing', empty)@ for an empty input, or
-- @('Just' rootCid, blocks)@ where @blocks@ contains all generated MST
-- node blocks keyed by their CIDs.
buildMST :: [(T.Text, CidBytes)] -> (Maybe CidBytes, BlockMap)
buildMST entries =
  case fromList entries of
    Nothing  -> (Nothing, Map.empty)
    Just mst -> (Just (rootCid mst), toBlockMap mst)
