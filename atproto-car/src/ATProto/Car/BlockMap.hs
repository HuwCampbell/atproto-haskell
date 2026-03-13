-- | Block map: a 'Map' from binary CIDs to raw block bytes.
module ATProto.Car.BlockMap
  ( BlockMap
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import ATProto.Car.Cid (CidBytes)

-- | A mapping from binary CIDs to the raw bytes of the corresponding block.
type BlockMap = Map.Map CidBytes BS.ByteString
