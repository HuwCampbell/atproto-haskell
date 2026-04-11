-- | Binary CIDv1 parsing and display.
--
-- A CIDv1 in binary form consists of:
--
-- 1. Version varint (always @0x01@)
-- 2. Codec varint   (@0x71@ for dag-cbor, @0x55@ for raw)
-- 3. Multihash:     hash-function-code varint + digest-length varint + digest bytes
--
-- For the common dag-cbor + sha2-256 case the total is 36 bytes:
-- @[0x01, 0x71, 0x12, 0x20] ++ 32-byte-sha256@
--
-- The display form uses multibase base32lower (codec letter @b@).
module ATProto.Car.Cid
  ( -- * Type
    CidBytes
    -- * Construction
  , cidForDagCbor
    -- * Parsing
  , parseCidFromBytes
    -- * Display
  , cidToText
  , textToCidBytes
    -- * Varint helpers (re-exported for the writer)
  , encodeVarint

    -- * Unsafe
  , unsafeCidBytes
  , unsafeRawCid
  ) where

import           Data.Bits  (shiftR, shiftL, (.|.), (.&.))
import           Data.Word  (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteArray  as BA
import qualified Data.Text       as T
import qualified Crypto.Hash     as H

-- | A CIDv1 stored as its raw binary representation.
newtype CidBytes = CidBytes { unsafeCidBytes :: BS.ByteString }
  deriving (Eq, Ord, Show)


unsafeRawCid :: BS.ByteString -> CidBytes
unsafeRawCid = CidBytes

-- ---------------------------------------------------------------------------
-- CID construction
-- ---------------------------------------------------------------------------

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
-- Varint helpers
-- ---------------------------------------------------------------------------

-- | Encode a non-negative integer as an unsigned LEB-128 varint.
encodeVarint :: Int -> Builder.Builder
encodeVarint n
  | n < 0x80  = Builder.word8 (fromIntegral n)
  | otherwise = Builder.word8 (fromIntegral (n .|. 0x80)) <> encodeVarint (n `shiftR` 7)

-- | Decode an unsigned LEB-128 varint from a 'BS.ByteString' starting at
-- @offset@.  Returns @(value, bytesConsumed)@ or an error.
decodeVarint :: BS.ByteString -> Int -> Either String (Int, Int)
decodeVarint bs start = loop 0 0 start
  where
    loop :: Int -> Int -> Int -> Either String (Int, Int)
    loop acc shift pos
      | pos >= BS.length bs = Left "varint: unexpected end of input"
      | otherwise =
          let b    = fromIntegral (BS.index bs pos) :: Int
              val  = acc .|. ((b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
             then Right (val, pos - start + 1)
             else loop val (shift + 7) (pos + 1)

-- ---------------------------------------------------------------------------
-- CID parsing
-- ---------------------------------------------------------------------------

-- | Parse a CIDv1 from raw bytes starting at @offset@.
--
-- Returns @(CidBytes, bytesConsumed)@ on success, or a human-readable error.
parseCidFromBytes :: BS.ByteString -> Int -> Either String (CidBytes, Int)
parseCidFromBytes bs off = do
  (ver, vLen) <- decodeVarint bs off
  if ver /= 1
    then Left ("CID: unsupported version " ++ show ver)
    else do
      let off1 = off + vLen
      (_codec, cLen) <- decodeVarint bs off1
      let off2 = off1 + cLen
      (_hfCode, hLen) <- decodeVarint bs off2
      let off3 = off2 + hLen
      (dLen, dlLen) <- decodeVarint bs off3
      let off4  = off3 + dlLen
          total = off4 - off + dLen
      if off + total > BS.length bs
        then Left "CID: input truncated before digest end"
        else Right (CidBytes (BS.take total (BS.drop off bs)), total)

-- ---------------------------------------------------------------------------
-- Display
-- ---------------------------------------------------------------------------

-- | Encode a 'CidBytes' as a multibase base32lower string (prefix @\'b\'@).
cidToText :: CidBytes -> T.Text
cidToText (CidBytes bs) = T.pack ('b' : encodeBase32Lower bs)

-- | Base-32 lower-case encoding without padding.
-- Uses the standard RFC 4648 base32 alphabet: @a-z2-7@.
encodeBase32Lower :: BS.ByteString -> String
encodeBase32Lower bs = go (BS.unpack bs) 0 0
  where
    -- | Lookup table: index 0-31 → base32 character
    b32 :: Int -> Char
    b32 n = "abcdefghijklmnopqrstuvwxyz234567" !! n

    -- | @go bytes bitsHeld accumulator@
    -- @bitsHeld@: how many bits are currently held in @accumulator@ (top-aligned)
    -- @accumulator@: an Int holding the pending bits in the most-significant positions
    go :: [Word8] -> Int -> Int -> String
    go []     held acc
      | held == 0 = []
      | otherwise =
          -- Flush: left-justify remaining bits into a 5-bit window
          let padded = acc `shiftL` (5 - held)
          in [b32 (padded .&. 0x1F)]
    go (w:ws) held acc =
      -- Merge the new byte into the accumulator (top-aligned)
      let acc'  = (acc `shiftL` 8) .|. fromIntegral w
          held' = held + 8
      in emit acc' held' ws

    -- | Emit as many 5-bit groups as possible, then recurse with @go@.
    emit :: Int -> Int -> [Word8] -> String
    emit acc held ws
      | held >= 5 =
          let held'  = held - 5
              nibble = (acc `shiftR` held') .&. 0x1F
          in b32 nibble : emit acc held' ws
      | otherwise = go ws held acc

-- | Parse a multibase base32lower CID string back to 'CidBytes'.
--
-- The string must start with @\'b\'@ (multibase base32lower prefix) followed
-- by base32lower-encoded bytes (alphabet @a-z2-7@, no padding).
textToCidBytes :: T.Text -> Either String CidBytes
textToCidBytes t = case T.uncons t of
  Nothing       -> Left "textToCidBytes: empty string"
  Just ('b', r) -> fmap CidBytes (decodeBase32Lower (T.unpack r))
  Just (c, _)   -> Left ("textToCidBytes: unknown multibase prefix " ++ show c)

-- | Decode a base32lower string (alphabet @a-z2-7@, no padding) to bytes.
decodeBase32Lower :: String -> Either String BS.ByteString
decodeBase32Lower s = do
  vals <- mapM charToVal s
  Right (BS.pack (collect vals 0 0))
  where
    charToVal :: Char -> Either String Int
    charToVal c
      | c >= 'a' && c <= 'z' = Right (fromEnum c - fromEnum 'a')
      | c >= '2' && c <= '7' = Right (fromEnum c - fromEnum '2' + 26)
      | otherwise             = Left ("decodeBase32Lower: invalid character " ++ show c)

    -- Accumulate 5-bit groups into bytes.  As per RFC 4648 §3.3 the final
    -- partial group of less than 8 bits is simply discarded (no padding).
    collect :: [Int] -> Int -> Int -> [Word8]
    collect []     _    _   = []
    collect (v:vs) held acc =
      let acc'  = (acc `shiftL` 5) .|. v
          held' = held + 5
      in if held' >= 8
           then let held'' = held' - 8
                    byte   = fromIntegral ((acc' `shiftR` held'') .&. 0xFF)
                in byte : collect vs held'' acc'
           else collect vs held' acc'
