module Test.ATProto.OAuth.DPoP (tests) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import ATProto.OAuth.DPoP

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | A generated DPoP key has a public JWK representation.
prop_dpopKeyHasPublicKey :: Property
prop_dpopKeyHasPublicKey = withTests 5 . property $ do
  key <- evalIO generateDpopKey
  case dpopPublicJwk key of
    Nothing -> fail "DPoP key has no public JWK"
    Just _  -> success

-- | A DPoP proof is a non-empty text string.
prop_dpopProofNonEmpty :: Property
prop_dpopProofNonEmpty = withTests 5 . property $ do
  key <- evalIO generateDpopKey
  result <- evalIO $ createDpopProof key DpopClaims
    { dcHtm   = "POST"
    , dcHtu   = "https://example.com/oauth/token"
    , dcNonce = Nothing
    , dcAth   = Nothing
    }
  case result of
    Left err    -> fail ("DPoP proof failed: " ++ err)
    Right proof -> assert (not (T.null proof))

-- | A DPoP proof has exactly two '.' separators (compact JWS = 3 segments).
prop_dpopProofCompactFormat :: Property
prop_dpopProofCompactFormat = withTests 5 . property $ do
  key <- evalIO generateDpopKey
  result <- evalIO $ createDpopProof key DpopClaims
    { dcHtm   = "POST"
    , dcHtu   = "https://bsky.social/oauth/token"
    , dcNonce = Nothing
    , dcAth   = Nothing
    }
  case result of
    Left err    -> fail ("DPoP proof failed: " ++ err)
    Right proof ->
      length (filter (== '.') (T.unpack proof)) === 2

-- | Two proofs for the same claims are different (random jti).
prop_dpopProofsDistinct :: Property
prop_dpopProofsDistinct = withTests 5 . property $ do
  key <- evalIO generateDpopKey
  let claims = DpopClaims
        { dcHtm   = "POST"
        , dcHtu   = "https://example.com/oauth/token"
        , dcNonce = Nothing
        , dcAth   = Nothing
        }
  r1 <- evalIO (createDpopProof key claims)
  r2 <- evalIO (createDpopProof key claims)
  case (r1, r2) of
    (Right p1, Right p2) -> p1 /== p2
    _                    -> fail "DPoP proof creation failed"

-- | A DPoP proof with nonce differs from one without.
prop_dpopNonceIncluded :: Property
prop_dpopNonceIncluded = withTests 3 . property $ do
  key <- evalIO generateDpopKey
  let base = DpopClaims
        { dcHtm   = "POST"
        , dcHtu   = "https://example.com/oauth/token"
        , dcNonce = Nothing
        , dcAth   = Nothing
        }
  r1 <- evalIO (createDpopProof key base)
  r2 <- evalIO (createDpopProof key base { dcNonce = Just "server-nonce-abc" })
  case (r1, r2) of
    (Right p1, Right p2) -> p1 /== p2
    _                    -> fail "DPoP proof creation failed"

-- | A JTI is 32 hex characters (16 random bytes).
prop_jtiFormat :: Property
prop_jtiFormat = withTests 10 . property $ do
  jti <- evalIO generateJti
  let hexChars = ("0123456789abcdef" :: String)
  T.length jti === 32
  assert (all (`elem` hexChars) (T.unpack jti))

-- | Two JTIs are always distinct.
prop_jtiDistinct :: Property
prop_jtiDistinct = withTests 10 . property $ do
  j1 <- evalIO generateJti
  j2 <- evalIO generateJti
  j1 /== j2

-- | The access token hash is 43 characters (base64url-unpadded SHA-256).
prop_athLength :: Property
prop_athLength = property $ do
  tok <- forAll $ fmap (T.pack . BC.unpack) (Gen.bytes (Range.linear 16 128))
  BC.length (accessTokenHash tok) === 43

tests :: Group
tests = Group "OAuth.DPoP"
  [ ("dpop key has public JWK",          prop_dpopKeyHasPublicKey)
  , ("dpop proof is non-empty",          prop_dpopProofNonEmpty)
  , ("dpop proof is compact JWS format", prop_dpopProofCompactFormat)
  , ("two proofs for same claims differ", prop_dpopProofsDistinct)
  , ("nonce changes proof",              prop_dpopNonceIncluded)
  , ("jti is 32 hex chars",              prop_jtiFormat)
  , ("jti values are distinct",          prop_jtiDistinct)
  , ("ath is 43 chars",                  prop_athLength)
  ]
