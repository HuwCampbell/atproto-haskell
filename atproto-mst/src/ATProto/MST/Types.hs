-- | MST error type, shared across all MST modules.
module ATProto.MST.Types
  ( MstError (..)
  ) where

import qualified Data.Text as T

-- | Errors from MST operations.
data MstError
  = MstNodeNotFound T.Text
    -- ^ A block referenced by CID was not found in the block map.
  | MstDecodeError T.Text
    -- ^ CBOR decoding of an MST node failed.
  deriving (Eq, Show)
