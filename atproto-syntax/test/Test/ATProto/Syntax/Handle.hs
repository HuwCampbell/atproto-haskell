module Test.ATProto.Syntax.Handle (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Data.Char (toLower)
import qualified Data.Text as T

import ATProto.Syntax.Handle

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | A single DNS label: 1–20 chars, alphanumeric / hyphens, not
-- starting or ending with a hyphen.
genLabel :: Gen T.Text
genLabel = do
  n <- Gen.int (Range.linear 1 20)
  if n == 1
    then T.singleton <$> Gen.alphaNum
    else do
      first  <- Gen.alphaNum
      middle <- Gen.text (Range.linear 0 (n - 2))
                  (Gen.element $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-")
      last'  <- Gen.alphaNum
      return $ T.pack [first] <> middle <> T.pack [last']

-- | A TLD: must start with an ASCII letter.
genTld :: Gen T.Text
genTld = do
  first <- Gen.element ['a'..'z']
  rest  <- Gen.text (Range.linear 0 10) Gen.alphaNum
  return $ T.pack (first : T.unpack rest)

-- | A valid handle: at least label.tld, with optional extra labels.
-- | A valid handle: always at least one domain label followed by the TLD
-- (giving at least one dot and two labels as required).
genValidHandle :: Gen T.Text
genValidHandle = do
  n      <- Gen.int (Range.linear 0 2)   -- 0..2 extra labels before the domain label
  extras <- Gen.list (Range.singleton n) genLabel
  dom    <- genLabel   -- mandatory domain label (e.g. "bsky")
  tld    <- genTld
  return $ T.intercalate "." (extras ++ [dom, tld])

-- | A handle whose TLD starts with a digit (invalid).
genTldStartsWithDigit :: Gen T.Text
genTldStartsWithDigit = do
  lbl  <- genLabel
  d    <- Gen.element ['0'..'9']
  rest <- Gen.text (Range.linear 0 5) Gen.alphaNum
  return $ lbl <> "." <> T.pack (d : T.unpack rest)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_validHandlesAccepted :: Property
prop_validHandlesAccepted = property $ do
  t <- forAll genValidHandle
  if T.length t <= 253
    then isValidHandle t === True
    else success   -- skip rare over-length results

prop_isValidHandleConsistent :: Property
prop_isValidHandleConsistent = property $ do
  t <- forAll genValidHandle
  case parseHandle t of
    Right _ -> isValidHandle t === True
    Left  _ -> isValidHandle t === False

prop_normaliseIsLowercase :: Property
prop_normaliseIsLowercase = property $ do
  t <- forAll genValidHandle
  case parseHandle t of
    Left  _ -> success
    Right h ->
      unHandle (normaliseHandle h) === T.map toLower (unHandle h)

prop_normaliseIdempotent :: Property
prop_normaliseIdempotent = property $ do
  t <- forAll genValidHandle
  case parseHandle t of
    Left  _ -> success
    Right h ->
      normaliseHandle (normaliseHandle h) === normaliseHandle h

prop_tldStartsWithDigitInvalid :: Property
prop_tldStartsWithDigitInvalid = property $ do
  t <- forAll genTldStartsWithDigit
  isValidHandle t === False

prop_singleLabelInvalid :: Property
prop_singleLabelInvalid = property $ do
  lbl <- forAll genLabel
  isValidHandle lbl === False

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Handle"
  [ ("valid handles are accepted",                    prop_validHandlesAccepted)
  , ("isValidHandle consistent with parseHandle",     prop_isValidHandleConsistent)
  , ("normalise produces lowercase",                  prop_normaliseIsLowercase)
  , ("normalise is idempotent",                       prop_normaliseIdempotent)
  , ("TLD starting with digit is invalid",            prop_tldStartsWithDigitInvalid)
  , ("single label (no dot) is invalid",              prop_singleLabelInvalid)
  ]
