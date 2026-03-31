-- | Tests for commit signature verification.
--
-- We use the Haskell crypto library to generate a real keypair, build the
-- canonical unsigned-commit encoding (replicating the internal logic of
-- 'verifyCommitSig'), sign it, and check that verification succeeds or fails
-- as expected.
module Test.ATProto.Repo.Verify.Commit (tests) where

import           Data.Bits            (xor)
import           Hedgehog
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Codec.CBOR.Encoding  as E
import qualified Codec.CBOR.Write     as W

import ATProto.Car.Cid            (CidBytes, parseCidFromBytes, unsafeCidBytes)
import ATProto.Crypto.Types       (Curve (..), Signature (..))
import ATProto.Crypto.EC          (generateKeyPair, sign)
import ATProto.Repo.Verify.Types  (Commit (..), VerifyError (..))
import ATProto.Repo.Verify.Commit (verifyCommitSig)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

dummyCid :: CidBytes
dummyCid =
  case parseCidFromBytes raw 0 of
    Right (c, _) -> c
    Left  err    -> error ("dummyCid: " ++ err)
  where
    raw = BS.pack
      [ 0x01, 0x71, 0x12, 0x20
      , 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b
      , 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde
      , 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21
      , 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13
      ]

-- | Build a 'Commit' with the given raw signature bytes.
mkCommit :: BS.ByteString -> Commit
mkCommit sig = Commit
  { commitDid     = "did:plc:testuser"
  , commitVersion = 3
  , commitRev     = "3jqfcqzm3fn2j"
  , commitPrev    = Nothing
  , commitData    = dummyCid
  , commitSig     = sig
  }

-- | Re-encode the unsigned commit in canonical DAG-CBOR.
--
-- This must match the encoding in 'ATProto.Repo.Verify.Commit.encodeUnsignedCommit'.
-- Canonical field order (length then lexicographic):
-- "did"(3), "rev"(3), "data"(4), "prev"(4), "version"(7)
canonicalEncoding :: Commit -> BS.ByteString
canonicalEncoding c =
  BL.toStrict . W.toLazyByteString $
    E.encodeMapLen 5
    <> E.encodeString "did"     <> E.encodeString (commitDid c)
    <> E.encodeString "rev"     <> E.encodeString (commitRev c)
    <> E.encodeString "data"    <> encTag42 (commitData c)
    <> E.encodeString "prev"    <> E.encodeNull
    <> E.encodeString "version" <> E.encodeInt (commitVersion c)
  where
    encTag42 cid =
      E.encodeTag 42 <> E.encodeBytes (BS.cons 0x00 (unsafeCidBytes cid))

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | A commit signed with the correct key verifies successfully.
prop_signAndVerify :: Property
prop_signAndVerify = property $ do
  (privKey, pubKey) <- evalIO $ generateKeyPair P256
  let base    = mkCommit (BS.replicate 64 0x00)
      payload = canonicalEncoding base
  Signature sigBytes <- evalIO $ sign privKey payload
  let commit = base { commitSig = sigBytes }
  case verifyCommitSig commit pubKey of
    Left err -> annotate (show err) >> failure
    Right () -> success

-- | A tampered signature is rejected with 'VerifyBadSig'.
prop_tamperedSig :: Property
prop_tamperedSig = property $ do
  (privKey, pubKey) <- evalIO $ generateKeyPair P256
  let base    = mkCommit (BS.replicate 64 0x00)
      payload = canonicalEncoding base
  Signature sigBytes <- evalIO $ sign privKey payload
  -- Flip the last byte of the signature
  let n        = BS.length sigBytes
      tampered = BS.take (n - 1) sigBytes <> BS.singleton (BS.last sigBytes `xorByte` 0xFF)
  let commit = base { commitSig = tampered }
  case verifyCommitSig commit pubKey of
    Left VerifyBadSig -> success
    other             -> annotate (show other) >> failure
  where
    xorByte a b = fromIntegral (fromIntegral a `xor` fromIntegral b :: Int)

-- | A signature over a different payload is rejected.
prop_wrongPayload :: Property
prop_wrongPayload = property $ do
  (privKey, pubKey) <- evalIO $ generateKeyPair P256
  -- Sign a different commit (different DID)
  let wrong = mkCommit (BS.replicate 64 0x00)
  Signature sigBytes <- evalIO $ sign privKey (canonicalEncoding wrong { commitDid = "did:plc:other" })
  let commit = wrong { commitSig = sigBytes }
  -- Verification uses commit.did = "did:plc:testuser", so payload differs
  -- Actually this test is tricky since the commit's did is "did:plc:testuser"
  -- but we signed "did:plc:other". They should not match.
  case verifyCommitSig commit pubKey of
    Left VerifyBadSig -> success
    other             -> annotate (show other) >> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.Repo.Verify.Commit"
  [ ("sign and verify roundtrip",       prop_signAndVerify)
  , ("tampered sig yields VerifyBadSig", prop_tamperedSig)
  , ("wrong payload yields VerifyBadSig", prop_wrongPayload)
  ]
