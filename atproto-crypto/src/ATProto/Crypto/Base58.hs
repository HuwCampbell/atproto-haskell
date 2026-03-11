-- | Base58btc encoding and decoding.
--
-- This is a pure Haskell 98 implementation of the base58btc alphabet used
-- by the AT Protocol multikey format (and by Bitcoin addresses).
--
-- Alphabet: @123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz@
-- (58 characters; uppercase I, O and lowercase l, 0 are omitted to avoid
-- visual ambiguity).
module ATProto.Crypto.Base58
  ( encodeBase58
  , decodeBase58
  ) where

import qualified Data.ByteString  as BS
import Data.List                  (elemIndex)

-- | The base58btc alphabet.
alphabet :: String
alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- | Encode a 'BS.ByteString' to its base58btc 'String' representation.
encodeBase58 :: BS.ByteString -> String
encodeBase58 bs =
  let leadingZeroCount = BS.length (BS.takeWhile (== 0) bs)
      n      = bsToInteger bs
      digits = integerToBase58Digits n
      prefix = replicate leadingZeroCount '1'   -- '1' represents a zero byte
  in prefix ++ map (alphabet !!) digits

-- | Decode a base58btc 'String' to a 'BS.ByteString'.
--
-- Returns 'Nothing' if the input contains characters outside the alphabet.
decodeBase58 :: String -> Maybe BS.ByteString
decodeBase58 s =
  let leadingOnes = length (takeWhile (== '1') s)
      rest        = dropWhile (== '1') s
  in case mapM (`elemIndex` alphabet) rest of
       Nothing     -> Nothing
       Just digits ->
         let n      = base58DigitsToInteger digits
             bytes  = integerToBS n
             prefix = BS.replicate leadingOnes 0
         in Just (prefix <> bytes)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Convert a 'BS.ByteString' to a non-negative 'Integer' (big-endian).
bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl (\acc b -> acc * 256 + fromIntegral b) 0

-- | Convert a non-negative 'Integer' to a minimal 'BS.ByteString' (big-endian).
integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.empty
integerToBS n = BS.pack (go n [])
  where
    go 0 acc = acc
    go x acc = go (x `div` 256) (fromIntegral (x `mod` 256) : acc)

-- | Decompose a non-negative integer into base-58 digits, most-significant first.
integerToBase58Digits :: Integer -> [Int]
integerToBase58Digits 0 = []
integerToBase58Digits x =
  integerToBase58Digits (x `div` 58) ++ [fromIntegral (x `mod` 58)]

-- | Reconstruct an integer from a list of base-58 digits (most-significant first).
base58DigitsToInteger :: [Int] -> Integer
base58DigitsToInteger = foldl (\acc d -> acc * 58 + fromIntegral d) 0
