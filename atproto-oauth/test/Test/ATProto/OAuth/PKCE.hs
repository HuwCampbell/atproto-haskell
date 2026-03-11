module Test.ATProto.OAuth.PKCE (tests) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import ATProto.OAuth.PKCE

-- ---------------------------------------------------------------------------
-- Test vectors from RFC 7636 §B
--
-- The RFC provides an example with:
--   code_verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
--   code_challenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
-- ---------------------------------------------------------------------------

-- | The example code_verifier from RFC 7636 §B.
rfc7636Verifier :: BS.ByteString
rfc7636Verifier = BC.pack "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

-- | The expected code_challenge for the above verifier.
rfc7636Challenge :: BS.ByteString
rfc7636Challenge = BC.pack "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

-- | The RFC 7636 test vector round-trips correctly.
prop_rfc7636 :: Property
prop_rfc7636 = withTests 1 . property $
  codeChallenge rfc7636Verifier === rfc7636Challenge

-- | The code challenge is 43 characters for any 32-byte verifier.
-- BASE64URL-unpadded of a 32-byte SHA-256 digest = 43 chars.
prop_challengeLength :: Property
prop_challengeLength = withTests 1 . property $
  BS.length (codeChallenge rfc7636Verifier) === 43

-- | Two different verifiers produce different challenges.
prop_challengeDistinct :: Property
prop_challengeDistinct = property $ do
  v1 <- forAll $ Gen.bytes (Range.singleton 32)
  v2 <- forAll $ Gen.bytes (Range.singleton 32)
  if v1 == v2
    then success  -- skip identical verifiers
    else codeChallenge v1 /== codeChallenge v2

-- | A generated code verifier is 43 characters long.
prop_verifierLength :: Property
prop_verifierLength = withTests 10 . property $ do
  v <- evalIO generateCodeVerifier
  BS.length v === 43

-- | A generated code verifier contains only base64url-safe characters.
prop_verifierChars :: Property
prop_verifierChars = withTests 10 . property $ do
  v <- evalIO generateCodeVerifier
  let safeChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '_']
  assert (all (`elem` safeChars) (BC.unpack v))

-- | The challenge of a generated verifier is also 43 characters.
prop_generatedPair :: Property
prop_generatedPair = withTests 10 . property $ do
  v <- evalIO generateCodeVerifier
  BS.length (codeChallenge v) === 43

tests :: Group
tests = Group "OAuth.PKCE"
  [ ("RFC 7636 test vector",          prop_rfc7636)
  , ("challenge length is 43",        prop_challengeLength)
  , ("distinct verifiers → distinct challenges", prop_challengeDistinct)
  , ("generated verifier length",     prop_verifierLength)
  , ("generated verifier chars",      prop_verifierChars)
  , ("generated pair: challenge 43",  prop_generatedPair)
  ]
