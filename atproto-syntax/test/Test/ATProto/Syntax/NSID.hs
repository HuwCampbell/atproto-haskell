module Test.ATProto.Syntax.NSID (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T

import ATProto.Syntax.NSID

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | A single authority/middle segment: starts with a letter, may contain
-- letters, digits, hyphens; must not start or end with a hyphen.
genSegment :: Gen T.Text
genSegment = do
  first <- Gen.element ['a'..'z']
  rest  <- Gen.text (Range.linear 0 10)
             (Gen.element $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-")
  return $ T.dropWhileEnd (== '-') (T.pack [first] <> rest)

-- | The name component: letters and digits only, no leading digit, no hyphen.
genName :: Gen T.Text
genName = do
  first <- Gen.element ['a'..'z']
  rest  <- Gen.text (Range.linear 0 15)
             (Gen.element $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
  return $ T.pack [first] <> rest

-- | A valid NSID: at least 2 authority segments + 1 name segment.
genValidNsid :: Gen T.Text
genValidNsid = do
  n    <- Gen.int (Range.linear 2 4)
  segs <- Gen.list (Range.singleton n) genSegment
  name <- genName
  return $ T.intercalate "." (segs ++ [name])

-- | An NSID with only two dot-separated parts (always invalid).
genTwoPart :: Gen T.Text
genTwoPart = do
  s1 <- genSegment
  s2 <- genSegment
  return $ s1 <> "." <> s2

-- | An NSID whose name component contains a hyphen (invalid).
genNameWithHyphen :: Gen T.Text
genNameWithHyphen = do
  seg1 <- genSegment
  seg2 <- genSegment
  pre  <- genName
  suf  <- genName
  return $ T.intercalate "." [seg1, seg2, pre <> "-" <> suf]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_validNsidsAccepted :: Property
prop_validNsidsAccepted = property $ do
  t <- forAll genValidNsid
  if T.length t > 317
    then success
    else case parseNSID t of
           Left err -> annotate err >> failure
           Right _  -> success

prop_parseRoundtrip :: Property
prop_parseRoundtrip = property $ do
  t <- forAll genValidNsid
  case parseNSID t of
    Left  _    -> success
    Right nsid ->
      T.intercalate "." (nsidSegments nsid) === t

prop_nameIsLastSegment :: Property
prop_nameIsLastSegment = property $ do
  t <- forAll genValidNsid
  case parseNSID t of
    Left  _    -> success
    Right nsid ->
      nsidName nsid === last (nsidSegments nsid)

prop_mkNsidRoundtrip :: Property
prop_mkNsidRoundtrip = property $ do
  t <- forAll genValidNsid
  case parseNSID t of
    Left  _    -> success
    Right nsid ->
      case mkNSID (nsidAuthority nsid) (nsidName nsid) of
        Left err    -> annotate err >> failure
        Right nsid' -> nsidSegments nsid' === nsidSegments nsid

prop_twoPartInvalid :: Property
prop_twoPartInvalid = property $ do
  t <- forAll genTwoPart
  isValidNSID t === False

prop_nameWithHyphenInvalid :: Property
prop_nameWithHyphenInvalid = property $ do
  t <- forAll genNameWithHyphen
  isValidNSID t === False

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "NSID"
  [ ("valid NSIDs are accepted",           prop_validNsidsAccepted)
  , ("parse roundtrip preserves text",     prop_parseRoundtrip)
  , ("nsidName is the last segment",       prop_nameIsLastSegment)
  , ("mkNSID roundtrip",                   prop_mkNsidRoundtrip)
  , ("two-part NSID is invalid",           prop_twoPartInvalid)
  , ("name with hyphen is invalid",        prop_nameWithHyphenInvalid)
  ]
