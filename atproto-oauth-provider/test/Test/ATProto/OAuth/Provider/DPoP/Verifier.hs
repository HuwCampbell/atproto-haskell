module Test.ATProto.OAuth.Provider.DPoP.Verifier (tests) where

import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import           Hedgehog

import ATProto.OAuth.DPoP              (generateDpopKey, createDpopProof,
                                        DpopClaims (..))
import ATProto.OAuth.Provider.DPoP.Nonce (newNonceState, nextNonce)
import ATProto.OAuth.Provider.DPoP.Verifier
import ATProto.OAuth.Provider.Types

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | A valid DPoP proof passes verification.
prop_validProofVerifies :: Property
prop_validProofVerifies = withTests 5 . property $ do
  key <- evalIO generateDpopKey
  let method = "POST"
      url    = "https://auth.example.com/oauth/token"
  eProof <- evalIO $ createDpopProof key DpopClaims
    { dcHtm = method, dcHtu = url, dcNonce = Nothing, dcAth = Nothing }
  case eProof of
    Left err -> fail ("DPoP proof creation failed: " ++ err)
    Right proofJwt -> do
      let headers = Map.singleton "dpop" proofJwt
      result <- evalIO $ verifyDpopProof method url headers Nothing Nothing
      case result of
        Left err       -> fail ("Verification failed: " ++ show err)
        Right Nothing  -> fail "Expected proof but got Nothing"
        Right (Just p) -> do
          assert (not (T.null (dpJti p)))
          assert (not (T.null (dpJkt p)))
          dpHtm p === method
          dpHtu p === url

-- | No DPoP header returns Nothing (not an error).
prop_noDpopHeader :: Property
prop_noDpopHeader = withTests 1 . property $ do
  let headers = Map.empty :: Map.Map T.Text T.Text
  result <- evalIO $ verifyDpopProof "GET" "https://example.com" headers Nothing Nothing
  case result of
    Right Nothing -> success
    _             -> fail "Expected Right Nothing for missing DPoP header"

-- | Empty DPoP header returns an error.
prop_emptyDpopHeader :: Property
prop_emptyDpopHeader = withTests 1 . property $ do
  let headers = Map.singleton "dpop" ""
  result <- evalIO $ verifyDpopProof "GET" "https://example.com" headers Nothing Nothing
  case result of
    Left (InvalidDpopProof _) -> success
    _                         -> fail "Expected InvalidDpopProof for empty header"

-- | Wrong HTTP method causes htm mismatch.
prop_htmMismatch :: Property
prop_htmMismatch = withTests 3 . property $ do
  key <- evalIO generateDpopKey
  eProof <- evalIO $ createDpopProof key DpopClaims
    { dcHtm = "POST", dcHtu = "https://example.com/token"
    , dcNonce = Nothing, dcAth = Nothing }
  case eProof of
    Left err -> fail ("DPoP proof creation failed: " ++ err)
    Right proofJwt -> do
      let headers = Map.singleton "dpop" proofJwt
      -- Verify with wrong method.
      result <- evalIO $ verifyDpopProof "GET" "https://example.com/token" headers Nothing Nothing
      case result of
        Left (InvalidDpopProof msg) -> assert (T.isInfixOf "htm" msg)
        _                           -> fail "Expected htm mismatch error"

-- | Wrong URL causes htu mismatch.
prop_htuMismatch :: Property
prop_htuMismatch = withTests 3 . property $ do
  key <- evalIO generateDpopKey
  eProof <- evalIO $ createDpopProof key DpopClaims
    { dcHtm = "POST", dcHtu = "https://example.com/token"
    , dcNonce = Nothing, dcAth = Nothing }
  case eProof of
    Left err -> fail ("DPoP proof creation failed: " ++ err)
    Right proofJwt -> do
      let headers = Map.singleton "dpop" proofJwt
      -- Verify with wrong URL.
      result <- evalIO $ verifyDpopProof "POST" "https://other.example.com/token" headers Nothing Nothing
      case result of
        Left (InvalidDpopProof msg) -> assert (T.isInfixOf "htu" msg)
        _                           -> fail "Expected htu mismatch error"

-- | Nonce is required when nonce state is enabled.
prop_nonceRequired :: Property
prop_nonceRequired = withTests 3 . property $ do
  nonceState <- evalIO $ newNonceState Nothing Nothing
  key <- evalIO generateDpopKey
  eProof <- evalIO $ createDpopProof key DpopClaims
    { dcHtm = "POST", dcHtu = "https://example.com/token"
    , dcNonce = Nothing, dcAth = Nothing }
  case eProof of
    Left err -> fail ("DPoP proof creation failed: " ++ err)
    Right proofJwt -> do
      let headers = Map.singleton "dpop" proofJwt
      result <- evalIO $ verifyDpopProof "POST" "https://example.com/token"
                           headers (Just nonceState) Nothing
      case result of
        Left UseDpopNonce -> success
        _                 -> fail "Expected UseDpopNonce error"

-- | Valid nonce passes verification.
prop_validNonce :: Property
prop_validNonce = withTests 3 . property $ do
  nonceState <- evalIO $ newNonceState Nothing Nothing
  nonce <- evalIO $ nextNonce nonceState
  key <- evalIO generateDpopKey
  eProof <- evalIO $ createDpopProof key DpopClaims
    { dcHtm = "POST", dcHtu = "https://example.com/token"
    , dcNonce = Just nonce, dcAth = Nothing }
  case eProof of
    Left err -> fail ("DPoP proof creation failed: " ++ err)
    Right proofJwt -> do
      let headers = Map.singleton "dpop" proofJwt
      result <- evalIO $ verifyDpopProof "POST" "https://example.com/token"
                           headers (Just nonceState) Nothing
      case result of
        Left err       -> fail ("Verification failed: " ++ show err)
        Right Nothing  -> fail "Expected proof but got Nothing"
        Right (Just _) -> success

-- | URL normalisation strips query and fragment.
prop_normalizeHtu :: Property
prop_normalizeHtu = withTests 1 . property $ do
  normalizeHtu "https://example.com/path?query=1#frag" === "https://example.com/path"
  normalizeHtu "https://example.com/path?query=1" === "https://example.com/path"
  normalizeHtu "https://example.com/path#frag" === "https://example.com/path"
  normalizeHtu "https://example.com/path" === "https://example.com/path"

tests :: Group
tests = Group "OAuth.Provider.DPoP.Verifier"
  [ ("valid proof verifies",              prop_validProofVerifies)
  , ("no DPoP header returns Nothing",    prop_noDpopHeader)
  , ("empty DPoP header errors",          prop_emptyDpopHeader)
  , ("htm mismatch detected",             prop_htmMismatch)
  , ("htu mismatch detected",             prop_htuMismatch)
  , ("nonce required when enabled",       prop_nonceRequired)
  , ("valid nonce passes",                prop_validNonce)
  , ("URL normalisation",                 prop_normalizeHtu)
  ]
