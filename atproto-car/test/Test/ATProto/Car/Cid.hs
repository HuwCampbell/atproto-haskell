module Test.ATProto.Car.Cid (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as BS
import qualified Data.Text       as T
import           Data.Bits       (shiftR, (.|.))
import           Data.Word       (Word8)

import ATProto.Car.Cid hiding (encodeVarint)

-- ---------------------------------------------------------------------------
-- Generator
-- ---------------------------------------------------------------------------

-- | Encode a non-negative integer as an unsigned LEB-128 varint.
encodeVarint :: Int -> [Word8]
encodeVarint n
  | n < 0x80  = [fromIntegral n]
  | otherwise = fromIntegral (n .|. 0x80) : encodeVarint (n `shiftR` 7)

-- | Generator for structurally valid CIDv1 bytes.
--
-- Produces a CIDv1 with:
--   * version = 1
--   * a codec from a small set of known multicodec codes (raw, dag-cbor)
--   * a hash-function code from known multihash codes
--     (sha2-256 0x12, sha2-512 0x14, keccak-256 0x1b)
--   * a random digest of 1–64 bytes
genCidBytes :: Gen CidBytes
genCidBytes = do
  codec  <- Gen.element [0x55, 0x71]        -- 0x55 = raw, 0x71 = dag-cbor
  hfCode <- Gen.element [0x12, 0x14, 0x1b]  -- 0x12 = sha2-256, 0x14 = sha2-512, 0x1b = keccak-256
  dLen   <- Gen.int (Range.linear 1 64)
  digest <- Gen.bytes (Range.singleton dLen)
  let header = BS.pack ([0x01]
                        ++ encodeVarint codec
                        ++ encodeVarint hfCode
                        ++ encodeVarint dLen)
  pure (CidBytes (header <> digest))

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Known CID bytes from Test.ATProto.Car.Parser
knownCidBytes :: BS.ByteString
knownCidBytes = BS.pack
  [ 0x01, 0x71, 0x12, 0x20
  , 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b
  , 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde
  , 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21
  , 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Any generated valid CIDv1 parses successfully and round-trips through
-- 'parseCidFromBytes': the returned bytes equal the original input.
prop_parseCidRoundtrip :: Property
prop_parseCidRoundtrip = property $ do
  cid <- forAll genCidBytes
  let bs    = unCidBytes cid
      total = BS.length bs
  case parseCidFromBytes bs 0 of
    Left err       -> do
      annotate err
      failure
    Right (cid', n) -> do
      n    === total
      cid' === cid

-- | Parsing at a non-zero offset works correctly: a random prefix is ignored
-- and the CID bytes returned match the generated CID.
prop_parseAtOffset :: Property
prop_parseAtOffset = property $ do
  cid    <- forAll genCidBytes
  prefix <- forAll $ Gen.bytes (Range.linear 1 8)
  let bs  = prefix <> unCidBytes cid
      off = BS.length prefix
  case parseCidFromBytes bs off of
    Left err        -> do
      annotate err
      failure
    Right (cid', n) -> do
      n    === BS.length (unCidBytes cid)
      cid' === cid

-- | Truncating a generated CID to fewer bytes than its length yields Left.
prop_truncated :: Property
prop_truncated = property $ do
  cid   <- forAll genCidBytes
  let bs    = unCidBytes cid
      total = BS.length bs
  -- Keep strictly fewer bytes than the full CID (at least header, less digest).
  keep  <- forAll $ Gen.int (Range.linear 0 (total - 1))
  case parseCidFromBytes (BS.take keep bs) 0 of
    Left _  -> success
    Right _ -> failure

-- | 'cidToText' always produces a string prefixed with @\'b\'@ (multibase
-- base32lower), for any generated CID.
prop_cidToTextPrefix :: Property
prop_cidToTextPrefix = property $ do
  cid <- forAll genCidBytes
  case T.uncons (cidToText cid) of
    Just ('b', _) -> success
    _             -> failure

-- | A version byte other than 1 is rejected.
prop_badVersion :: Property
prop_badVersion = property $ do
  cid <- forAll genCidBytes
  -- Overwrite the first byte (version) with something other than 0x01.
  ver <- forAll $ Gen.word8 (Range.linear 0x02 0xFF)
  let bs = BS.cons ver (BS.tail (unCidBytes cid))
  case parseCidFromBytes bs 0 of
    Left _  -> success
    Right _ -> failure

-- | 'cidToText' followed by 'textToCidBytes' is the identity on any generated
-- valid CIDv1.
prop_cidToTextRoundtrip :: Property
prop_cidToTextRoundtrip = property $ do
  cid <- forAll genCidBytes
  textToCidBytes (cidToText cid) === Right cid

-- | textToCidBytes decodes the known CID string to the known bytes.
prop_textToCidBytesKnownValue :: Property
prop_textToCidBytesKnownValue = property $ do
  textToCidBytes "bafyreifqwkmiw256ojf2zws6tzjeonw6bpd5vza4i22ccpcq4hjv2ts7cm"
    === Right (CidBytes knownCidBytes)

-- | A wrong multibase prefix yields Left.
prop_textToCidBytesInvalidPrefix :: Property
prop_textToCidBytesInvalidPrefix = property $ do
  case textToCidBytes "zfoo" of
    Left _  -> success
    Right _ -> failure

-- | Invalid base32 characters yield Left.
prop_textToCidBytesInvalidChar :: Property
prop_textToCidBytesInvalidChar = property $ do
  case textToCidBytes "b!!!!" of
    Left _  -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.Car.Cid"
  [ ("parse/roundtrip: generated valid CID",      prop_parseCidRoundtrip)
  , ("parse at non-zero offset",                  prop_parseAtOffset)
  , ("truncated input yields Left",               prop_truncated)
  , ("cidToText starts with 'b'",                 prop_cidToTextPrefix)
  , ("bad version byte rejected",                 prop_badVersion)
  , ("cidToText/textToCidBytes roundtrip",        prop_cidToTextRoundtrip)
  , ("textToCidBytes known value",                prop_textToCidBytesKnownValue)
  , ("textToCidBytes invalid prefix yields Left", prop_textToCidBytesInvalidPrefix)
  , ("textToCidBytes invalid char yields Left",   prop_textToCidBytesInvalidChar)
  ]
