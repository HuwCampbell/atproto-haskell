-- | Shared DAG-CBOR helpers for CID encoding and decoding.
--
-- CBOR tag 42 is used by the IPLD\/DAG-CBOR specification to represent
-- content identifiers (CIDs).  The payload is a byte string consisting
-- of a @0x00@ identity multibase prefix followed by the raw CID bytes.
--
-- These helpers are used by the CAR parser\/writer, MST encoder, commit
-- encoder, and signature verification code.
module ATProto.Car.DagCbor
  ( -- * Encoding
    encodeCidTag42
  , encodeNullableCidTag42
    -- * Decoding
  , decodeCidTag42
  , decodeNullableCidTag42
    -- * Skipping unknown fields
  , skipValue
  ) where

import qualified Data.ByteString      as BS
import qualified Codec.CBOR.Encoding  as E
import qualified Codec.CBOR.Decoding  as D

import ATProto.Car.Cid (CidBytes (..), parseCidFromBytes)

-- ---------------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------------

-- | Encode a CID as CBOR tag 42 with a @0x00@ identity multibase prefix.
encodeCidTag42 :: CidBytes -> E.Encoding
encodeCidTag42 (CidBytes raw) =
  E.encodeTag 42 <> E.encodeBytes (BS.cons 0x00 raw)

-- | Encode an optional CID: CBOR null for 'Nothing', tag 42 for 'Just'.
encodeNullableCidTag42 :: Maybe CidBytes -> E.Encoding
encodeNullableCidTag42 Nothing    = E.encodeNull
encodeNullableCidTag42 (Just cid) = encodeCidTag42 cid

-- ---------------------------------------------------------------------------
-- Decoding
-- ---------------------------------------------------------------------------

-- | Decode a CID from CBOR tag 42.
--
-- Expects @tag(42, bytes(0x00 ++ raw_cid))@.
decodeCidTag42 :: D.Decoder s CidBytes
decodeCidTag42 = do
  tag <- D.decodeTag
  if tag /= 42
    then fail ("expected CBOR tag 42, got " ++ show tag)
    else do
      raw <- D.decodeBytes
      let stripped = if not (BS.null raw) && BS.head raw == 0x00
                       then BS.tail raw
                       else raw
      case parseCidFromBytes stripped 0 of
        Left err       -> fail err
        Right (cid, _) -> return cid

-- | Decode an optional CID: CBOR null → 'Nothing', tag 42 → 'Just'.
decodeNullableCidTag42 :: D.Decoder s (Maybe CidBytes)
decodeNullableCidTag42 = do
  ty <- D.peekTokenType
  case ty of
    D.TypeNull -> D.decodeNull >> return Nothing
    _          -> Just <$> decodeCidTag42

-- ---------------------------------------------------------------------------
-- Skipping
-- ---------------------------------------------------------------------------

-- | Skip an arbitrary CBOR value.
--
-- Handles null, bool, unsigned\/negative integers, bytes, strings,
-- fixed-length lists\/maps, and tags.  This is sufficient for skipping
-- over unknown commit and MST node fields.
skipValue :: D.Decoder s ()
skipValue = do
  ty <- D.peekTokenType
  case ty of
    D.TypeNull    -> D.decodeNull
    D.TypeBool    -> D.decodeBool  >> return ()
    D.TypeUInt    -> D.decodeWord  >> return ()
    D.TypeNInt    -> D.decodeNegWord >> return ()
    D.TypeBytes   -> D.decodeBytes >> return ()
    D.TypeString  -> D.decodeString >> return ()
    D.TypeListLen -> do
      len <- D.decodeListLen
      mapM_ (\_ -> skipValue) [1..len]
    D.TypeMapLen  -> do
      len <- D.decodeMapLen
      mapM_ (\_ -> D.decodeString >> skipValue) [1..len]
    D.TypeTag     -> do
      _ <- D.decodeTag
      skipValue
    _ -> fail ("skipValue: unsupported token type " ++ show ty)
