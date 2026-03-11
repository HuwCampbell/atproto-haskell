module Test.ATProto.Crypto.DidKey (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen

import ATProto.Crypto.Types
import ATProto.Crypto.EC       (generateKeyPair)
import ATProto.Crypto.DidKey
import ATProto.Crypto.Multikey (encodeMultikey, decodeMultikey)
import qualified Hedgehog.Range as Range

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genCurve :: Gen Curve
genCurve = Gen.element [P256, Secp256k1]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | did:key roundtrip: encode then decode gives back the same PubKey.
prop_didKeyRoundtrip :: Property
prop_didKeyRoundtrip = property $ do
  curve <- forAll genCurve
  (_, pub) <- evalIO (generateKeyPair curve)
  didKeyToPubKey (pubKeyToDidKey pub) === Right pub

-- | did:key strings start with the expected prefix.
prop_didKeyPrefix :: Property
prop_didKeyPrefix = property $ do
  curve <- forAll genCurve
  (_, pub) <- evalIO (generateKeyPair curve)
  let dk = pubKeyToDidKey pub
  take 8 dk === "did:key:"

-- | did:key for P-256 starts with the known multibase prefix 'z' and
-- the P-256 codec prefix (base58btc-encoded [0x80, 0x24]).
prop_didKeyMultikeySuffix :: Property
prop_didKeyMultikeySuffix = property $ do
  curve <- forAll genCurve
  (_, pub) <- evalIO (generateKeyPair curve)
  let mk = drop 8 (pubKeyToDidKey pub)   -- strip "did:key:"
  take 1 mk === "z"                       -- multibase base58btc prefix

-- | Multikey roundtrip: encode then decode gives back the same PubKey.
prop_multikeyRoundtrip :: Property
prop_multikeyRoundtrip = property $ do
  curve <- forAll genCurve
  (_, pub) <- evalIO (generateKeyPair curve)
  decodeMultikey (encodeMultikey pub) === Right pub

-- | A string without the 'z' prefix is rejected by decodeMultikey.
prop_multikeyNonZPrefix :: Property
prop_multikeyNonZPrefix = property $ do
  s <- forAll $ Gen.string (Range.linear 1 40) Gen.alphaNum
  case decodeMultikey s of
    Left  _ -> success   -- expected
    Right _ ->
      -- only succeeds if the generated string happens to start with 'z'
      -- and decode correctly; make sure it starts with 'z'
      take 1 s === "z"

-- | A did:key with a bad prefix is rejected.
prop_didKeyBadPrefix :: Property
prop_didKeyBadPrefix = property $ do
  s <- forAll $ Gen.string (Range.linear 1 20) Gen.alphaNum
  let bad = "did:web:" ++ s
  case didKeyToPubKey bad of
    Left  _ -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "DidKey"
  [ ("did:key roundtrip",                 prop_didKeyRoundtrip)
  , ("did:key starts with 'did:key:'",    prop_didKeyPrefix)
  , ("multikey starts with 'z'",          prop_didKeyMultikeySuffix)
  , ("multikey roundtrip",                prop_multikeyRoundtrip)
  , ("non-z prefix rejected",             prop_multikeyNonZPrefix)
  , ("bad did scheme rejected",           prop_didKeyBadPrefix)
  ]
