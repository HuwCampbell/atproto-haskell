module Test.ATProto.Syntax.TID (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T

import ATProto.Syntax.TID

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

validFirstChars :: String
validFirstChars = "234567abcdefghij"

base32Alphabet :: String
base32Alphabet = "234567abcdefghijklmnopqrstuvwxyz"

-- | A generator for syntactically valid TID strings (exactly 13 chars).
genValidTid :: Gen T.Text
genValidTid = do
  first <- Gen.element validFirstChars
  rest  <- Gen.string (Range.singleton 12) (Gen.element base32Alphabet)
  return $ T.pack (first : rest)

-- | A generator for strings that are the wrong length (anything but 13).
genWrongLength :: Gen T.Text
genWrongLength = do
  n <- Gen.filter (/= 13) (Gen.int (Range.linear 0 30))
  Gen.text (Range.singleton n) (Gen.element base32Alphabet)

-- | 13-char strings whose first character is outside the valid set.
genBadFirstChar :: Gen T.Text
genBadFirstChar = do
  first <- Gen.element ("0189" :: String)
  rest  <- Gen.string (Range.singleton 12) (Gen.element base32Alphabet)
  return $ T.pack (first : rest)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_validTidsAccepted :: Property
prop_validTidsAccepted = property $ do
  t <- forAll genValidTid
  isValidTID t === True

prop_parseRoundtrip :: Property
prop_parseRoundtrip = property $ do
  t <- forAll genValidTid
  case parseTID t of
    Left err  -> annotate err >> failure
    Right tid -> unTID tid === t

prop_wrongLengthInvalid :: Property
prop_wrongLengthInvalid = property $ do
  t <- forAll genWrongLength
  isValidTID t === False

prop_badFirstCharInvalid :: Property
prop_badFirstCharInvalid = property $ do
  t <- forAll genBadFirstChar
  isValidTID t === False

prop_uppercaseInvalid :: Property
prop_uppercaseInvalid = property $ do
  first <- forAll $ Gen.element ['A'..'Z']
  rest  <- forAll $ Gen.string (Range.singleton 12) (Gen.element base32Alphabet)
  isValidTID (T.pack (first : rest)) === False

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "TID"
  [ ("valid TIDs are accepted",            prop_validTidsAccepted)
  , ("parse roundtrip preserves text",     prop_parseRoundtrip)
  , ("wrong-length strings are invalid",   prop_wrongLengthInvalid)
  , ("bad first character is invalid",     prop_badFirstCharInvalid)
  , ("uppercase characters are invalid",   prop_uppercaseInvalid)
  ]
