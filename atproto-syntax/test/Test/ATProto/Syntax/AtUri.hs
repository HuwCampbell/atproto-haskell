module Test.ATProto.Syntax.AtUri (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T

import ATProto.Syntax.AtUri

-- ---------------------------------------------------------------------------
-- Sub-generators
-- ---------------------------------------------------------------------------

genDid :: Gen T.Text
genDid = do
  method <- Gen.text (Range.linear 1 8) Gen.lower
  body   <- Gen.text (Range.linear 1 20)
              (Gen.element $ ['a'..'z'] ++ ['0'..'9'] ++ "-.")
  return $ "did:" <> method <> ":" <> body

genHandle :: Gen T.Text
genHandle = do
  label <- Gen.text (Range.linear 1 10) Gen.alphaNum
  first <- Gen.element ['a'..'z']
  rest  <- Gen.text (Range.linear 0 5) Gen.alphaNum
  return $ label <> "." <> T.pack (first : T.unpack rest)

genAuthority :: Gen T.Text
genAuthority = Gen.choice [genDid, genHandle]

genNsid :: Gen T.Text
genNsid = do
  a <- Gen.element ['a'..'z']
  b <- Gen.text (Range.linear 1 8) Gen.alphaNum
  c <- Gen.element ['a'..'z']
  d <- Gen.text (Range.linear 0 8) Gen.alphaNum
  return $ T.pack [a] <> "." <> b <> "." <> T.pack [c] <> d

genRkey :: Gen T.Text
genRkey = Gen.text (Range.linear 1 20)
            (Gen.element $ ['a'..'z'] ++ ['0'..'9'])

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Authority-only URIs survive a parse-render round-trip.
prop_authorityRoundtrip :: Property
prop_authorityRoundtrip = property $ do
  auth <- forAll genAuthority
  let t = "at://" <> auth
  case parseAtUri t of
    Left err -> annotate err >> failure
    Right u  -> renderAtUri u === t

-- | The collection is preserved after parsing.
prop_collectionPreserved :: Property
prop_collectionPreserved = property $ do
  auth <- forAll genAuthority
  col  <- forAll genNsid
  let t = "at://" <> auth <> "/" <> col
  case parseAtUri t of
    Left err -> annotate err >> failure
    Right u  -> atUriCollection u === Just col

-- | Both collection and rkey are populated after parsing a full URI.
prop_rkeyPreserved :: Property
prop_rkeyPreserved = property $ do
  auth <- forAll genAuthority
  col  <- forAll genNsid
  rk   <- forAll genRkey
  let t = "at://" <> auth <> "/" <> col <> "/" <> rk
  case parseAtUri t of
    Left err -> annotate err >> failure
    Right u  -> do
      atUriCollection u /== Nothing
      atUriRkey       u /== Nothing

-- | render . parse . render is stable (idempotent rendering).
prop_renderParseRoundtrip :: Property
prop_renderParseRoundtrip = property $ do
  auth <- forAll genAuthority
  col  <- forAll genNsid
  rk   <- forAll genRkey
  let t = "at://" <> auth <> "/" <> col <> "/" <> rk
  case parseAtUri t of
    Left  _  -> success
    Right u1 ->
      case parseAtUri (renderAtUri u1) of
        Left err -> annotate err >> failure
        Right u2 -> renderAtUri u2 === renderAtUri u1

-- | @at://@ with no authority is always invalid.
prop_emptyAuthorityInvalid :: Property
prop_emptyAuthorityInvalid = property $
  isValidAtUri "at://" === False

-- | Four path segments (auth/col/rkey/extra) are always invalid.
prop_tooManySegmentsInvalid :: Property
prop_tooManySegmentsInvalid = property $ do
  auth  <- forAll genAuthority
  col   <- forAll genNsid
  rk    <- forAll genRkey
  extra <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
  let t = "at://" <> auth <> "/" <> col <> "/" <> rk <> "/" <> extra
  isValidAtUri t === False

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "AtUri"
  [ ("authority-only URIs roundtrip",          prop_authorityRoundtrip)
  , ("collection is preserved after parse",    prop_collectionPreserved)
  , ("rkey is present after full parse",       prop_rkeyPreserved)
  , ("render-parse roundtrip is stable",       prop_renderParseRoundtrip)
  , ("empty authority is invalid",             prop_emptyAuthorityInvalid)
  , ("too many path segments is invalid",      prop_tooManySegmentsInvalid)
  ]
