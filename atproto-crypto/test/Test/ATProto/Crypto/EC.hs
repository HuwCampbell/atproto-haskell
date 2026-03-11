module Test.ATProto.Crypto.EC (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as BS

import ATProto.Crypto.Types
import ATProto.Crypto.EC

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genCurve :: Gen Curve
genCurve = Gen.element [P256, Secp256k1]

genMsg :: Gen BS.ByteString
genMsg = Gen.bytes (Range.linear 0 256)

-- | Curve order n for low-S threshold check.
curveN :: Curve -> Integer
curveN P256      = 0xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551
curveN Secp256k1 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

bsToInt :: BS.ByteString -> Integer
bsToInt = BS.foldl (\acc b -> acc * 256 + fromIntegral b) 0

sigS :: Signature -> Integer
sigS (Signature bs) = bsToInt (BS.drop 32 bs)

-- | Flip s → n-s to produce a high-S version of a signature.
flipS :: Curve -> Signature -> Signature
flipS curve (Signature bs) =
  let n = curveN curve
      r = BS.take 32 bs
      s = bsToInt (BS.drop 32 bs)
      s' = n - s
      -- Re-encode s' as 32 big-endian bytes
      s'Bytes = intToBS32 s'
  in Signature (r <> s'Bytes)
  where
    intToBS32 n0 = BS.pack (go (32 :: Int) n0 [])
      where
        go 0 _ acc = acc
        go k x acc = go (k-1) (x `div` 256) (fromIntegral (x `mod` 256) : acc)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Signing then verifying in Strict mode succeeds.
prop_signVerifyStrict :: Property
prop_signVerifyStrict = property $ do
  curve  <- forAll genCurve
  msg    <- forAll genMsg
  (priv, pub) <- evalIO (generateKeyPair curve)
  sig    <- evalIO (sign priv msg)
  verify Strict pub msg sig === True

-- | Signing then verifying in Lax mode also succeeds.
prop_signVerifyLax :: Property
prop_signVerifyLax = property $ do
  curve  <- forAll genCurve
  msg    <- forAll genMsg
  (priv, pub) <- evalIO (generateKeyPair curve)
  sig    <- evalIO (sign priv msg)
  verify Lax pub msg sig === True

-- | Generated signatures always satisfy low-S (s <= n/2).
prop_signAlwaysLowS :: Property
prop_signAlwaysLowS = property $ do
  curve  <- forAll genCurve
  msg    <- forAll genMsg
  (priv, _) <- evalIO (generateKeyPair curve)
  sig    <- evalIO (sign priv msg)
  let n = curveN curve
      s = sigS sig
  assert (s >= 1)
  assert (s <= n `div` 2)

-- | A high-S signature is rejected in Strict mode but accepted in Lax mode.
prop_highSRejectedStrict :: Property
prop_highSRejectedStrict = property $ do
  curve  <- forAll genCurve
  msg    <- forAll genMsg
  (priv, pub) <- evalIO (generateKeyPair curve)
  sig    <- evalIO (sign priv msg)
  let highSSig = flipS curve sig
      n        = curveN curve
  -- Verify that our flip actually produced a high-S signature.
  assert (sigS highSSig > n `div` 2)
  -- Strict mode must reject it.
  verify Strict pub msg highSSig === False
  -- Lax mode must still accept it (ECDSA-valid, just high-S).
  verify Lax pub msg highSSig === True

-- | A wrong message gives a failed verification.
prop_wrongMsgFails :: Property
prop_wrongMsgFails = property $ do
  curve  <- forAll genCurve
  msg1   <- forAll genMsg
  msg2   <- forAll $ Gen.filter (/= msg1) genMsg
  (priv, pub) <- evalIO (generateKeyPair curve)
  sig    <- evalIO (sign priv msg1)
  verify Strict pub msg2 sig === False

-- | A signature from a different keypair is rejected.
prop_wrongKeyFails :: Property
prop_wrongKeyFails = property $ do
  curve  <- forAll genCurve
  msg    <- forAll genMsg
  (priv1, _)    <- evalIO (generateKeyPair curve)
  (_,     pub2) <- evalIO (generateKeyPair curve)
  sig    <- evalIO (sign priv1 msg)
  verify Strict pub2 msg sig === False

-- | importPrivKey accepts valid scalars.
prop_importPrivKeyValid :: Property
prop_importPrivKeyValid = property $ do
  curve  <- forAll genCurve
  (priv, _) <- evalIO (generateKeyPair curve)
  importPrivKey curve (privKeyBytes priv) === Right priv

-- | importPrivKey rejects a 31-byte input.
prop_importPrivKeyBadLength :: Property
prop_importPrivKeyBadLength = property $ do
  curve <- forAll genCurve
  bs    <- forAll $ Gen.bytes (Range.singleton 31)
  case importPrivKey curve bs of
    Left  _ -> success
    Right _ -> failure

-- | derivePublicKey is consistent with generateKeyPair.
prop_derivePublicKeyConsistent :: Property
prop_derivePublicKeyConsistent = property $ do
  curve <- forAll genCurve
  (priv, pub) <- evalIO (generateKeyPair curve)
  derivePublicKey priv === pub

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "EC"
  [ ("sign then verify Strict succeeds",     prop_signVerifyStrict)
  , ("sign then verify Lax succeeds",        prop_signVerifyLax)
  , ("generated sigs always have low-S",     prop_signAlwaysLowS)
  , ("high-S rejected Strict, pass Lax",     prop_highSRejectedStrict)
  , ("wrong message fails verification",     prop_wrongMsgFails)
  , ("wrong key fails verification",         prop_wrongKeyFails)
  , ("importPrivKey accepts valid scalar",   prop_importPrivKeyValid)
  , ("importPrivKey rejects bad length",     prop_importPrivKeyBadLength)
  , ("derivePublicKey consistent",           prop_derivePublicKeyConsistent)
  ]
