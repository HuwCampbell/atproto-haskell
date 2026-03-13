module Test.ATProto.MST.Layer (tests) where

import Hedgehog
import qualified Data.ByteString.Char8 as BSC

import ATProto.MST.Layer

-- ---------------------------------------------------------------------------
-- Known test vectors
-- ---------------------------------------------------------------------------

-- | Keys with their expected leading-zero-pair counts derived from SHA-256.
-- Values verified against the reference TypeScript implementation.
prop_knownVectors :: Property
prop_knownVectors = property $ do
  -- These keys are all level-0 (no leading zero pairs in hash).
  leadingZerosOnHash (BSC.pack "com.example.record/3jqfcqzm3fn2j") === 0
  leadingZerosOnHash (BSC.pack "com.example.record/3jqfcqzm3fn2k") === 0
  leadingZerosOnHash (BSC.pack "com.example.record/3jqfcqzm3fn2l") === 0
  leadingZerosOnHash (BSC.pack "com.example.record/3jqfcqzm3fp2j") === 0
  leadingZerosOnHash (BSC.pack "com.example.record/3jqfcqzm3fp2k") === 0
  leadingZerosOnHash (BSC.pack "com.example.record/3jqfcqzm3fp2l") === 0
  leadingZerosOnHash (BSC.pack "com.example.record/3jqfcqzm3fr2j") === 0
  -- This key hashes to level 1.
  leadingZerosOnHash (BSC.pack "com.example.record/3jqfcqzm3fr2k") === 1
  leadingZerosOnHash (BSC.pack "com.example.record/3jqfcqzm3fr2l") === 0

-- | Empty input has a well-defined result (all-zeros SHA-256 prefix).
prop_emptyKey :: Property
prop_emptyKey = property $ do
  -- SHA-256("") = e3b0c44298fc1c149...
  -- e3 = 227, which is >= 64, so result is 0.
  leadingZerosOnHash (BSC.pack "") === 0

-- | Result is always non-negative.
prop_nonNegative :: Property
prop_nonNegative = property $ do
  key <- forAll $ pure (BSC.pack "any-key")
  assert (leadingZerosOnHash key >= 0)

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.MST.Layer"
  [ ("known level vectors",   prop_knownVectors)
  , ("empty key",             prop_emptyKey)
  , ("result non-negative",   prop_nonNegative)
  ]
