module Test.ATProto.Car.Cid (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as BS
import qualified Data.Text       as T

import ATProto.Car.Cid

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Minimal valid dag-cbor sha2-256 CID bytes:
--   [0x01, 0x71, 0x12, 0x20] ++ 32 zero bytes
minimalCidBytes :: BS.ByteString
minimalCidBytes = BS.pack ([0x01, 0x71, 0x12, 0x20] ++ replicate 32 0x00)

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

-- | Parsing known-good CID bytes succeeds and consumes exactly 36 bytes.
prop_parseKnownCid :: Property
prop_parseKnownCid = property $ do
  case parseCidFromBytes minimalCidBytes 0 of
    Left err       -> do
      annotate err
      failure
    Right (cid, n) -> do
      n === 36
      unCidBytes cid === minimalCidBytes

-- | Parsing at non-zero offset works correctly.
prop_parseAtOffset :: Property
prop_parseAtOffset = property $ do
  prefix <- forAll $ Gen.bytes (Range.linear 1 8)
  let bs = prefix <> minimalCidBytes
  let off = BS.length prefix
  case parseCidFromBytes bs off of
    Left err       -> do
      annotate err
      failure
    Right (cid, n) -> do
      n === 36
      unCidBytes cid === minimalCidBytes

-- | Truncated input yields Left.
prop_truncated :: Property
prop_truncated = property $ do
  -- Take only 10 bytes of the 36-byte CID — digest will be incomplete.
  let bs = BS.take 10 minimalCidBytes
  case parseCidFromBytes bs 0 of
    Left _  -> success
    Right _ -> failure

-- | cidToText produces a string starting with 'b' (multibase base32lower).
prop_cidToTextPrefix :: Property
prop_cidToTextPrefix = property $ do
  let cid = CidBytes minimalCidBytes
  let t   = cidToText cid
  case T.uncons t of
    Just ('b', _) -> success
    _             -> failure

-- | A version byte other than 1 is rejected.
prop_badVersion :: Property
prop_badVersion = property $ do
  let bs = BS.cons 0x02 (BS.tail minimalCidBytes)
  case parseCidFromBytes bs 0 of
    Left _  -> success
    Right _ -> failure

-- | cidToText followed by textToCidBytes is the identity on CidBytes.
prop_cidToTextRoundtrip :: Property
prop_cidToTextRoundtrip = property $ do
  bs <- forAll $ Gen.bytes (Range.linear 1 64)
  let cid = CidBytes bs
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
  [ ("parse known CID bytes",                     prop_parseKnownCid)
  , ("parse at non-zero offset",                  prop_parseAtOffset)
  , ("truncated input yields Left",               prop_truncated)
  , ("cidToText starts with 'b'",                 prop_cidToTextPrefix)
  , ("bad version byte rejected",                 prop_badVersion)
  , ("cidToText/textToCidBytes roundtrip",        prop_cidToTextRoundtrip)
  , ("textToCidBytes known value",                prop_textToCidBytesKnownValue)
  , ("textToCidBytes invalid prefix yields Left", prop_textToCidBytesInvalidPrefix)
  , ("textToCidBytes invalid char yields Left",   prop_textToCidBytesInvalidChar)
  ]
