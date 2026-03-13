-- | MST layer computation.
--
-- The layer (depth) of a key in the MST is determined by hashing the key
-- bytes with SHA-256 and counting leading 2-bit zero pairs in the hash.
module ATProto.MST.Layer
  ( leadingZerosOnHash
  ) where

import           Data.Word                    (Word8)
import qualified Data.ByteString              as BS
import qualified Crypto.Hash                  as H
import qualified Data.ByteArray               as BA

-- | Hash @key@ with SHA-256 and count the number of leading 2-bit zero pairs.
--
-- For each byte in the hash (MSB first):
--
-- * If byte >= 64 (top 2 bits non-zero): stop
-- * If byte >= 16 (top 4 bits zero, next 2 non-zero): count += 1, stop
-- * If byte >= 4  (top 6 bits zero, next 2 non-zero): count += 2, stop
-- * If byte > 0   (top 7 bits zero, last bit non-zero): count += 3, stop
-- * If byte == 0  (all 8 bits zero): count += 4, continue next byte
leadingZerosOnHash :: BS.ByteString -> Int
leadingZerosOnHash key =
  let digest = H.hash key :: H.Digest H.SHA256
      bytes  = BA.unpack digest :: [Word8]
  in go bytes 0
  where
    go :: [Word8] -> Int -> Int
    go []     acc = acc
    go (b:bs) acc
      | b >= 64   = acc
      | b >= 16   = acc + 1
      | b >= 4    = acc + 2
      | b >  0    = acc + 3
      | otherwise = go bs (acc + 4)
