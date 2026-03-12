module ATProto.Ipld where

import qualified Data.Text as T
import           Data.ByteString (ByteString)
import           Data.Word (Word64)
import           Data.Vector (Vector)
import           Data.Map (Map)

-- | Ipld type for atproto
data Ipld
  -- | Reprseents a unit value.
  = Null
  -- | Represents a boolean value.
  | Bool Bool
  -- | Represents an integer.
  | Integer Word64
  -- | Represents a floating point value.
  | Float Double
  -- | Represents an UTF-8 string.
  | String T.Text
  -- | Represents a sequence of bytes.
  | Bytes ByteString
  -- | Represents a list.
  | List (Vector Ipld)
  -- | Represents a map of strings.
  | Map (Map T.Text Ipld)
  -- | Represents a map of integers.
--   | Link(Cid)
  deriving (Eq, Show)
