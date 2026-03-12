module Test.ATProto.Syntax.RecordKey (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T

import ATProto.Syntax.RecordKey

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

allowedChars :: String
allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_~.:-"

-- | A generator for syntactically valid record keys.
genValidRecordKey :: Gen T.Text
genValidRecordKey =
  Gen.filter (\t -> t /= "." && t /= "..") $
    Gen.text (Range.linear 1 100) (Gen.element allowedChars)

-- | A generator for record keys that are too long (> 512).
genTooLong :: Gen T.Text
genTooLong =
  Gen.text (Range.linear 513 600) (Gen.element allowedChars)

-- | A generator for strings containing a disallowed character.
genWithDisallowedChar :: Gen T.Text
genWithDisallowedChar = do
  prefix <- Gen.text (Range.linear 0 10) (Gen.element allowedChars)
  bad    <- Gen.element ("!@#$^&*()+= \t" :: String)
  suffix <- Gen.text (Range.linear 0 10) (Gen.element allowedChars)
  return $ prefix <> T.singleton bad <> suffix

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_validKeysAccepted :: Property
prop_validKeysAccepted = property $ do
  t <- forAll genValidRecordKey
  isValidRecordKey t === True

prop_parseRoundtrip :: Property
prop_parseRoundtrip = property $ do
  t <- forAll genValidRecordKey
  case parseRecordKey t of
    Left err -> annotate err >> failure
    Right rk -> unRecordKey rk === t

prop_emptyStringInvalid :: Property
prop_emptyStringInvalid = property $
  isValidRecordKey "" === False

prop_dotInvalid :: Property
prop_dotInvalid = property $
  isValidRecordKey "." === False

prop_dotDotInvalid :: Property
prop_dotDotInvalid = property $
  isValidRecordKey ".." === False

prop_tooLongInvalid :: Property
prop_tooLongInvalid = property $ do
  t <- forAll genTooLong
  isValidRecordKey t === False

prop_disallowedCharInvalid :: Property
prop_disallowedCharInvalid = property $ do
  t <- forAll genWithDisallowedChar
  isValidRecordKey t === False

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "RecordKey"
  [ ("valid record keys are accepted",          prop_validKeysAccepted)
  , ("parse roundtrip preserves text",          prop_parseRoundtrip)
  , ("empty string is invalid",                 prop_emptyStringInvalid)
  , ("'.' is invalid",                          prop_dotInvalid)
  , ("'..' is invalid",                         prop_dotDotInvalid)
  , ("keys > 512 chars are invalid",            prop_tooLongInvalid)
  , ("disallowed characters make key invalid",  prop_disallowedCharInvalid)
  ]
