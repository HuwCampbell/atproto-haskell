-- | Core IPLD value types for the AT Protocol data model.
--
-- The 'LexValue' type is the central intermediate representation used by
-- both the JSON and CBOR layers. Codecs produce and consume 'LexValue'
-- values; the wire-format modules then serialise 'LexValue' to bytes.
module ATProto.Ipld.Value
  ( -- * Content identifier
    Cid (..)
    -- * Blob reference
  , BlobRef (..)
    -- * The central IR
  , LexValue (..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Data.ByteString (ByteString)
import           Data.Int        (Int64)

-- | A content identifier (CID).
--
-- Stored as its string representation for now; this can be strengthened to
-- a proper multihash\/multibase type in the future without breaking the
-- public API.
newtype Cid = Cid { cidText :: T.Text }
  deriving (Eq, Ord, Show)

-- | A reference to an uploaded blob.
--
-- Corresponds to the @blob@ $type in the AT Protocol data model:
--
-- @
-- { "$type": "blob"
-- , "ref":      { "$link": "\<cid\>" }
-- , "mimeType": "image\/jpeg"
-- , "size":     12345
-- }
-- @
data BlobRef = BlobRef
  { blobRefCid      :: !Cid
    -- ^ The CID of the blob content.
  , blobRefMimeType :: !T.Text
    -- ^ MIME type, e.g. @\"image\/jpeg\"@.
  , blobRefSize     :: !Int64
    -- ^ Size of the blob in bytes.
  } deriving (Eq, Show)

-- | The AT Protocol data model value type.
--
-- This mirrors the runtime @LexValue@ from the upstream JavaScript
-- implementation and is used as the pivot between all serialisation formats:
--
-- @
-- Haskell type a
--     ↕  Codec.writer \/ Codec.decoder
-- LexValue
--     ↕  Json.hs         ↕  Cbor.hs
-- Aeson.Value          CBOR.Term
-- @
--
-- Note that there is no 'Float' constructor: the AT Protocol Lexicon type
-- system does not allow floating-point numbers in record fields.  CBOR input
-- that contains floats should be rejected at decode time.
data LexValue
  = LexNull
    -- ^ A null \/ absent value.
  | LexBool   !Bool
    -- ^ A boolean value.
  | LexInt    !Int64
    -- ^ A 64-bit signed integer.
  | LexString !T.Text
    -- ^ A UTF-8 string.
  | LexBytes  !ByteString
    -- ^ A raw byte array.
  | LexLink   !Cid
    -- ^ An IPLD CID link.
  | LexBlob   !BlobRef
    -- ^ A reference to an uploaded blob.
  | LexArray  ![LexValue]
    -- ^ An ordered sequence of values.
  | LexObject !(Map.Map T.Text LexValue)
    -- ^ A string-keyed map of values.
  deriving (Eq, Show)
