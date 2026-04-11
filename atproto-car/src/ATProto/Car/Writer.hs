-- | CAR v1 binary format writer.
--
-- Produces Content Addressable aRchive (CAR) v1 byte strings from a root
-- CID list and a block map.  This is the inverse of 'ATProto.Car.Parser'.
--
-- = CAR v1 layout
--
-- 1. Varint: byte length of the header block.
-- 2. Header block: a DAG-CBOR map @{ version: 1, roots: [CID, ...] }@.
-- 3. Zero or more data blocks, each:
--    a. Varint: @len(cid_bytes) + len(block_bytes)@.
--    b. Raw CID bytes.
--    c. Block bytes.
module ATProto.Car.Writer
  ( -- * Writers
    writeCar
  , writeCarWithRoot
  ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.Map.Strict         as Map
import qualified Codec.CBOR.Encoding     as E
import qualified Codec.CBOR.Write        as W

import ATProto.Car.Cid       (CidBytes, encodeVarint, unsafeCidBytes)
import ATProto.Car.BlockMap  (BlockMap)
import ATProto.Car.DagCbor   (encodeCidTag42)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Encode a CAR v1 file from a list of root CIDs and a block map.
--
-- The blocks are written in the ascending CID order of the underlying
-- 'Map'.  Every CID mentioned as a root should be present in the block
-- map, but this is not enforced.
writeCar :: [CidBytes] -> BlockMap -> BL.ByteString
writeCar roots blocks =
  let hdrBytes  = encodeHeader roots
      hdrLen    = encodeVarint (BS.length hdrBytes)
      blockSeq  = encodeBlocks blocks
      builder   = hdrLen <> Builder.byteString hdrBytes <> blockSeq
  in
    Builder.toLazyByteString builder

-- | Encode a CAR v1 file with exactly one root CID.
writeCarWithRoot :: CidBytes -> BlockMap -> BL.ByteString
writeCarWithRoot root = writeCar [root]

-- ---------------------------------------------------------------------------
-- Header encoding
-- ---------------------------------------------------------------------------

-- | Encode the CAR v1 header as DAG-CBOR.
--
-- @{ \"version\": 1, \"roots\": [tag42(cid), ...] }@
encodeHeader :: [CidBytes] -> BS.ByteString
encodeHeader roots =
  W.toStrictByteString $ mconcat
    [ E.encodeMapLen 2
    , E.encodeString "version"
    , E.encodeWord 1
    , E.encodeString "roots"
    , E.encodeListLen (fromIntegral (length roots))
    , mconcat [ encodeCidTag42 cid | cid <- roots ]
    ]

-- ---------------------------------------------------------------------------
-- Block sequence encoding
-- ---------------------------------------------------------------------------

-- | Encode all blocks as the data section of a CAR file.
encodeBlocks :: BlockMap -> Builder
encodeBlocks = Map.foldlWithKey' encodeBlock mempty

-- | Encode a single block entry: @varint(len) ++ cid_bytes ++ block_bytes@.
encodeBlock :: Builder -> CidBytes -> BS.ByteString -> Builder
encodeBlock acc cid blockBytes =
  let cidRaw   = unsafeCidBytes cid
      entryLen = BS.length cidRaw + BS.length blockBytes
      entry    = encodeVarint entryLen <> Builder.byteString cidRaw <> Builder.byteString blockBytes
  in acc <> entry
