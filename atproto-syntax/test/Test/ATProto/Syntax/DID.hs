module Test.ATProto.Syntax.DID (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T

import ATProto.Syntax.DID

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | Generate a syntactically valid DID string.
genValidDid :: Gen T.Text
genValidDid = do
  method <- Gen.text (Range.linear 1 10) Gen.lower
  body   <- Gen.text (Range.linear 1 40)
              (Gen.element $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "._-")
  return $ T.concat ["did:", method, ":", body]

-- | Generate a string that intentionally lacks the @did:@ prefix.
genNoPrefixDid :: Gen T.Text
genNoPrefixDid = do
  t <- Gen.text (Range.linear 1 30) Gen.alphaNum
  return ("x:" <> t)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Every string produced by 'genValidDid' should parse and roundtrip.
prop_validDidsRoundtrip :: Property
prop_validDidsRoundtrip = property $ do
  t <- forAll genValidDid
  case parseDID t of
    Left err -> annotate err >> failure
    Right d  -> unDID d === t

-- | 'isValidDID' returns 'True' for every generated valid DID.
prop_isValidDIDConsistent :: Property
prop_isValidDIDConsistent = property $ do
  t <- forAll genValidDid
  isValidDID t === True

-- | Strings without the @did:@ prefix are always rejected.
prop_noPrefixAlwaysInvalid :: Property
prop_noPrefixAlwaysInvalid = property $ do
  t <- forAll genNoPrefixDid
  isValidDID t === False

-- | A DID longer than 2048 characters is always rejected.
prop_tooLongAlwaysInvalid :: Property
prop_tooLongAlwaysInvalid = property $ do
  extra <- forAll $ Gen.text (Range.linear 1 500) Gen.alphaNum
  let t = "did:plc:" <> T.replicate 2042 "a" <> extra
  assert (T.length t > 2048)
  isValidDID t === False

-- | A DID ending in @:@ is always rejected.
prop_endsWithColonInvalid :: Property
prop_endsWithColonInvalid = property $ do
  method <- forAll $ Gen.text (Range.linear 1 8) Gen.lower
  let t = "did:" <> method <> ":"
  isValidDID t === False

-- | DID method must be all lower-case letters; upper-case methods fail.
prop_upperMethodInvalid :: Property
prop_upperMethodInvalid = property $ do
  method <- forAll $ Gen.text (Range.linear 1 8) Gen.upper
  let t = "did:" <> method <> ":abc"
  isValidDID t === False

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "DID"
  [ ("valid DIDs parse and roundtrip",     prop_validDidsRoundtrip)
  , ("isValidDID consistent with parseDID", prop_isValidDIDConsistent)
  , ("missing did: prefix always invalid",  prop_noPrefixAlwaysInvalid)
  , ("DID > 2048 chars always invalid",     prop_tooLongAlwaysInvalid)
  , ("DID ending with ':' always invalid",  prop_endsWithColonInvalid)
  , ("uppercase method always invalid",     prop_upperMethodInvalid)
  ]
