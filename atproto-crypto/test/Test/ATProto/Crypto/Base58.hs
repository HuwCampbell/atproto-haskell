module Test.ATProto.Crypto.Base58 (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as BS

import ATProto.Crypto.Base58

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genBytes :: Gen BS.ByteString
genBytes = Gen.bytes (Range.linear 0 64)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | decode . encode == id  (full roundtrip)
prop_roundtrip :: Property
prop_roundtrip = property $ do
  bs <- forAll genBytes
  decodeBase58 (encodeBase58 bs) === Just bs

-- | Leading zero bytes are preserved through the roundtrip.
prop_leadingZeros :: Property
prop_leadingZeros = property $ do
  n    <- forAll $ Gen.int (Range.linear 0 8)
  rest <- forAll $ Gen.bytes (Range.linear 0 20)
  let bs = BS.replicate n 0 <> rest
  decodeBase58 (encodeBase58 bs) === Just bs

-- | Characters outside the alphabet are rejected.
prop_invalidCharsRejected :: Property
prop_invalidCharsRejected = property $ do
  bad <- forAll $ Gen.element ("0OIl" :: String)
  decodeBase58 [bad] === Nothing

-- | Known-good vectors (empty, single zero byte, two zero bytes).
prop_knownVectors :: Property
prop_knownVectors = property $ do
  encodeBase58 BS.empty   === ""
  encodeBase58 (BS.pack [0])   === "1"
  encodeBase58 (BS.pack [0, 0]) === "11"
  decodeBase58 ""   === Just BS.empty
  decodeBase58 "1"  === Just (BS.pack [0])
  decodeBase58 "11" === Just (BS.pack [0, 0])

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Base58"
  [ ("encode/decode roundtrip",           prop_roundtrip)
  , ("leading zero bytes preserved",      prop_leadingZeros)
  , ("invalid characters rejected",       prop_invalidCharsRejected)
  , ("known encoding vectors",            prop_knownVectors)
  ]
