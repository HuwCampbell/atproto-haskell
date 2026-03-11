-- | Multibase / multikey encoding helpers.
--
-- The AT Protocol encodes public keys as "multikeys" – a multibase
-- base58btc-encoded byte string where the first two bytes are a varint
-- codec prefix identifying the curve and key type.
--
-- Codec prefixes (unsigned varint):
--
--   * @0x1200@ – P-256 compressed public key (\"p256-pub\")
--   * @0xe701@ – secp256k1 compressed public key (\"secp256k1-pub\")
module ATProto.Crypto.Multikey
  ( -- * Codec prefixes
    p256PubPrefix
  , secp256k1PubPrefix
    -- * Encoding / decoding
  , encodeMultikey
  , decodeMultikey
  ) where

import qualified Data.ByteString as BS

import ATProto.Crypto.Types

-- | Varint-encoded codec prefix for a compressed P-256 public key.
p256PubPrefix :: BS.ByteString
p256PubPrefix = BS.pack [0x80, 0x24]   -- 0x1200 in unsigned-varint LE

-- | Varint-encoded codec prefix for a compressed secp256k1 public key.
secp256k1PubPrefix :: BS.ByteString
secp256k1PubPrefix = BS.pack [0xe7, 0x01]  -- 0x0131 ... see multiformats

-- | Prepend the appropriate codec prefix to a compressed public-key point.
--
-- The resulting bytes are intended to be base58btc-multibase encoded
-- (prefix @z@) to produce a multikey string.
encodeMultikey :: PubKey -> BS.ByteString
encodeMultikey (PubKey curve bytes) =
  prefix <> bytes
  where
    prefix = case curve of
      P256      -> p256PubPrefix
      Secp256k1 -> secp256k1PubPrefix

-- | Strip the codec prefix from a raw multikey byte string and return the
-- curve and compressed key bytes.
--
-- Returns 'Left' when the prefix is unrecognised.
decodeMultikey :: BS.ByteString -> Either String PubKey
decodeMultikey bs
  | p256PubPrefix `BS.isPrefixOf` bs =
      Right $ PubKey P256 (BS.drop (BS.length p256PubPrefix) bs)
  | secp256k1PubPrefix `BS.isPrefixOf` bs =
      Right $ PubKey Secp256k1 (BS.drop (BS.length secp256k1PubPrefix) bs)
  | otherwise =
      Left "Unrecognised multikey codec prefix"
