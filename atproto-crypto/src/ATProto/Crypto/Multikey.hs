-- | Multikey encoding for the AT Protocol.
--
-- A multikey is the @z@-prefixed base58btc encoding of a codec-prefixed
-- compressed public key.  The @z@ prefix is the multibase identifier for
-- base58btc.
--
-- Codec prefixes (little-endian unsigned varint):
--
--   * @[0x80, 0x24]@ – P-256 compressed public key
--   * @[0xe7, 0x01]@ – secp256k1 compressed public key
module ATProto.Crypto.Multikey
  ( -- * Codec prefixes
    p256Prefix
  , secp256k1Prefix
    -- * Encoding / decoding
  , encodeMultikey
  , decodeMultikey
  ) where

import qualified Data.ByteString as BS

import ATProto.Crypto.Base58
import ATProto.Crypto.Types

-- | Codec prefix bytes for a P-256 compressed public key.
--
-- Encodes the multicodec value @0x1200@ in unsigned-varint little-endian.
p256Prefix :: BS.ByteString
p256Prefix = BS.pack [0x80, 0x24]

-- | Codec prefix bytes for a secp256k1 compressed public key.
--
-- Encodes the multicodec value @0xe7@ (231) in unsigned-varint: the
-- continuation bit is set on the first byte, giving @[0xe7, 0x01]@.
secp256k1Prefix :: BS.ByteString
secp256k1Prefix = BS.pack [0xe7, 0x01]

-- | Encode a 'PubKey' as a multikey string: @\"z\" ++ base58btc(prefix || key)@.
encodeMultikey :: PubKey -> String
encodeMultikey (PubKey curve bytes) =
  'z' : encodeBase58 (prefix <> bytes)
  where
    prefix = case curve of
      P256      -> p256Prefix
      Secp256k1 -> secp256k1Prefix

-- | Decode a multikey string into a 'PubKey'.
--
-- Returns 'Left' if the string is not a well-formed multikey for either
-- of the supported curves.
decodeMultikey :: String -> Either String PubKey
decodeMultikey ('z' : rest) =
  case decodeBase58 rest of
    Nothing -> Left "Multikey: base58btc decoding failed"
    Just bs
      | p256Prefix `BS.isPrefixOf` bs ->
          let keyBytes = BS.drop (BS.length p256Prefix) bs
          in if BS.length keyBytes == 33
             then Right (PubKey P256 keyBytes)
             else Left "Multikey: P-256 key payload is not 33 bytes"
      | secp256k1Prefix `BS.isPrefixOf` bs ->
          let keyBytes = BS.drop (BS.length secp256k1Prefix) bs
          in if BS.length keyBytes == 33
             then Right (PubKey Secp256k1 keyBytes)
             else Left "Multikey: secp256k1 key payload is not 33 bytes"
      | otherwise ->
          Left "Multikey: unrecognised codec prefix"
decodeMultikey _ = Left "Multikey: missing multibase 'z' prefix"
