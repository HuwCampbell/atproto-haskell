module ATProto.Cid where

import qualified Data.Text as T
import           Data.ByteString (ByteString)
import           Data.Word (Word64)
import           Data.Vector (Vector)
import           Data.Map (Map)
import           Data.Multihash (Multihash, Multihashable)
import qualified Data.Multihash as Multihash


data Version
  = V0
  | V1
  deriving (Eq, Show, Ord)

data Codec
    = Raw
    | DagProtobuf
    | DagCbor
    | GitRaw
    deriving (Eq, Show, Ord, Generic)

instance Hashable Codec
instance NFData   Codec

-- | A Content IDentifier.
--
-- * 'V0' 'CID's are merely SHA256 hashes, base58-encoded using the bitcoin
-- alphabet. The 'Codec' is implicitly 'DagProtobuf'.
-- * 'V1' 'CID's may use any 'Multihash', and any of the supported 'Codec's.
data CID =
  CID {
    cidVersion :: Version
  , cidCodec   :: Codec
  , cidHash    :: Multihash
  } deriving (Eq, Ord, Generic)


-- | Create a 'V1' 'CID'.
newCidV1 :: Multihashable a => Codec -> Digest a -> CID
newCidV1 codec dig = CID
    { cidVersion = V1
    , cidCodec   = codec
    , cidHash    = Multihash.fromDigest dig
    }