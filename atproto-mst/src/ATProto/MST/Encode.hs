-- | MST node CBOR encoding and CID computation.
--
-- Provides the inverse of 'ATProto.MST.Node.decodeNode': encoding a
-- 'NodeData' to deterministic DAG-CBOR bytes and computing the CIDv1
-- for the resulting block.
module ATProto.MST.Encode
  ( -- * Encoding
    encodeNode
    -- * CID computation (re-exported from "ATProto.Car.Cid")
  , cidForDagCbor
  ) where

import qualified Data.ByteString     as BS
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Write    as W

import ATProto.Car.Cid    (cidForDagCbor)
import ATProto.Car.DagCbor (encodeCidTag42, encodeNullableCidTag42)
import ATProto.MST.Node   (NodeData (..), TreeEntry (..))

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

-- ---------------------------------------------------------------------------
-- Internal CBOR encoding
-- ---------------------------------------------------------------------------

nodeEncoding :: NodeData -> E.Encoding
nodeEncoding (NodeData mLeft entries) =
     E.encodeMapLen 2
  <> E.encodeString "l"
  <> encodeNullableCidTag42 mLeft
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
  <> encodeCidTag42 value
  <> E.encodeString "t"
  <> encodeNullableCidTag42 mRight
