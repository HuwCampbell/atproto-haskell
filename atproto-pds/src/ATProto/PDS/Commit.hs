-- | AT Protocol commit creation and signing.
--
-- Provides functions for creating signed v3 commits as used by the AT
-- Protocol repository format.
--
-- A commit is a CBOR map with six fields.  The signing payload is the
-- DAG-CBOR encoding of the five non-signature fields in canonical order
-- (by key-byte-length, then lexicographic):
--
-- @did@ (3), @rev@ (3), @data@ (4), @prev@ (4), @version@ (7)
--
-- The signed commit adds @sig@ (3) in canonical position:
--
-- @did@ (3), @rev@ (3), @sig@ (3), @data@ (4), @prev@ (4), @version@ (7)
module ATProto.PDS.Commit
  ( -- * Creation
    createSignedCommit
    -- * Encoding helpers
  , encodeSignedCommit
  , encodeUnsignedCommit
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Codec.CBOR.Encoding  as E
import qualified Codec.CBOR.Write     as W

import ATProto.Car.Cid      (CidBytes, cidForDagCbor)
import ATProto.Car.DagCbor  (encodeCidTag42, encodeNullableCidTag42)
import ATProto.Crypto.EC    (sign)
import ATProto.Crypto.Types (PrivKey, Signature (..))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Create a signed v3 commit and return @(commitCid, commitBytes)@.
--
-- The caller supplies the repository DID, signing key, TID revision
-- string, optional previous commit CID, and the MST root CID.
createSignedCommit
  :: T.Text          -- ^ Repository DID
  -> PrivKey         -- ^ Signing key
  -> T.Text          -- ^ Revision (TID)
  -> Maybe CidBytes  -- ^ Previous commit CID ('Nothing' for the first commit)
  -> CidBytes        -- ^ MST data root CID
  -> IO (CidBytes, BS.ByteString)
createSignedCommit did key rev prev dataRoot = do
  let unsigned = encodeUnsignedCommit did rev prev dataRoot
  Signature sigBytes <- sign key unsigned
  let signed    = encodeSignedCommit did rev sigBytes prev dataRoot
      commitCid = cidForDagCbor signed
  return (commitCid, signed)

-- ---------------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------------

-- | Encode the unsigned commit payload (5 fields, no @sig@).
--
-- Field order (canonical DAG-CBOR):
-- @did@ (3), @rev@ (3), @data@ (4), @prev@ (4), @version@ (7)
encodeUnsignedCommit
  :: T.Text          -- ^ DID
  -> T.Text          -- ^ Revision
  -> Maybe CidBytes  -- ^ Previous commit
  -> CidBytes        -- ^ MST data root
  -> BS.ByteString
encodeUnsignedCommit did rev prev dataRoot =
  BL.toStrict . W.toLazyByteString $
       E.encodeMapLen 5
    <> E.encodeString "did"
    <> E.encodeString did
    <> E.encodeString "rev"
    <> E.encodeString rev
    <> E.encodeString "data"
    <> encodeCidTag42 dataRoot
    <> E.encodeString "prev"
    <> encodeNullableCidTag42 prev
    <> E.encodeString "version"
    <> E.encodeInt 3

-- | Encode the full signed commit (6 fields, including @sig@).
--
-- Field order (canonical DAG-CBOR):
-- @did@ (3), @rev@ (3), @sig@ (3), @data@ (4), @prev@ (4), @version@ (7)
encodeSignedCommit
  :: T.Text          -- ^ DID
  -> T.Text          -- ^ Revision
  -> BS.ByteString   -- ^ Signature bytes (64 bytes, r||s)
  -> Maybe CidBytes  -- ^ Previous commit
  -> CidBytes        -- ^ MST data root
  -> BS.ByteString
encodeSignedCommit did rev sigBytes prev dataRoot =
  BL.toStrict . W.toLazyByteString $
       E.encodeMapLen 6
    <> E.encodeString "did"
    <> E.encodeString did
    <> E.encodeString "rev"
    <> E.encodeString rev
    <> E.encodeString "sig"
    <> E.encodeBytes sigBytes
    <> E.encodeString "data"
    <> encodeCidTag42 dataRoot
    <> E.encodeString "prev"
    <> encodeNullableCidTag42 prev
    <> E.encodeString "version"
    <> E.encodeInt 3
