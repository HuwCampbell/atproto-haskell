-- | MST node CBOR encoding and CID computation.
--
-- Provides the inverse of 'ATProto.MST.Node.decodeNode': encoding a
-- 'NodeData' to deterministic DAG-CBOR bytes and computing the CIDv1
-- for the resulting block.
module ATProto.MST.Encode
  ( -- * Encoding
    encodeNode
    -- * CID computation
  , cidForDagCbor
  ) where

import qualified Data.ByteString     as BS
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Write    as W
import qualified Crypto.Hash         as H
import qualified Data.ByteArray      as BA

import ATProto.Car.Cid  (CidBytes (..))
import ATProto.MST.Node (NodeData (..), TreeEntry (..))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Encode an MST node to deterministic DAG-CBOR bytes.
--
-- The encoding follows the AT Protocol reference field ordering:
--
--  * Node map keys: @\"l\"@, @\"e\"@
--  * Entry map keys: @\"p\"@, @\"k\"@, @\"v\"@, @\"t\"@
--
-- This matches the upstream TypeScript implementation and is required
-- for CID compatibility.
encodeNode :: NodeData -> BS.ByteString
encodeNode = W.toStrictByteString . nodeEncoding

-- | Compute a CIDv1 for a raw DAG-CBOR block.
--
-- The CID uses codec @0x71@ (dag-cbor) and hash @sha2-256@
-- (@0x12@, 32-byte digest).
cidForDagCbor :: BS.ByteString -> CidBytes
cidForDagCbor raw =
  let digest    = H.hash raw :: H.Digest H.SHA256
      hashBytes = BS.pack (BA.unpack digest)
      header    = BS.pack [0x01, 0x71, 0x12, 0x20]
  in CidBytes (header <> hashBytes)

-- ---------------------------------------------------------------------------
-- Internal CBOR encoding
-- ---------------------------------------------------------------------------

nodeEncoding :: NodeData -> E.Encoding
nodeEncoding (NodeData mLeft entries) =
     E.encodeMapLen 2
  <> E.encodeString "l"
  <> encodeNullableCid mLeft
  <> E.encodeString "e"
  <> encodeEntryList entries

encodeEntryList :: [TreeEntry] -> E.Encoding
encodeEntryList entries =
     E.encodeListLen (fromIntegral (length entries))
  <> mconcat (map encodeTreeEntry entries)

encodeTreeEntry :: TreeEntry -> E.Encoding
encodeTreeEntry (TreeEntry prefix suffix value mRight) =
     E.encodeMapLen 4
  <> E.encodeString "p"
  <> E.encodeWord (fromIntegral prefix)
  <> E.encodeString "k"
  <> E.encodeBytes suffix
  <> E.encodeString "v"
  <> encodeCid value
  <> E.encodeString "t"
  <> encodeNullableCid mRight

-- | Encode a CID as CBOR tag 42 with a @0x00@ multibase identity prefix.
encodeCid :: CidBytes -> E.Encoding
encodeCid (CidBytes raw) =
  E.encodeTag 42 <> E.encodeBytes (BS.cons 0x00 raw)

-- | Encode an optional CID: CBOR null for 'Nothing', tag 42 for 'Just'.
encodeNullableCid :: Maybe CidBytes -> E.Encoding
encodeNullableCid Nothing    = E.encodeNull
encodeNullableCid (Just cid) = encodeCid cid
