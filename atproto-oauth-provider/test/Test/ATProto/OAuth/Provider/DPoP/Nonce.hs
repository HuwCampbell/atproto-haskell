module Test.ATProto.OAuth.Provider.DPoP.Nonce (tests) where

import qualified Data.ByteString       as BS
import qualified Data.Text             as T
import           Hedgehog

import ATProto.OAuth.Provider.DPoP.Nonce

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | A freshly generated nonce is non-empty.
prop_nonceNonEmpty :: Property
prop_nonceNonEmpty = withTests 5 . property $ do
  state <- evalIO $ newNonceState Nothing Nothing
  nonce <- evalIO $ nextNonce state
  assert (not (T.null nonce))

-- | A freshly issued nonce passes validation.
prop_nonceRoundTrip :: Property
prop_nonceRoundTrip = withTests 5 . property $ do
  state <- evalIO $ newNonceState Nothing Nothing
  nonce <- evalIO $ nextNonce state
  valid <- evalIO $ checkNonce state nonce
  assert valid

-- | An arbitrary string does not pass validation.
prop_invalidNonceRejected :: Property
prop_invalidNonceRejected = withTests 10 . property $ do
  state <- evalIO $ newNonceState Nothing Nothing
  -- Fetch a real nonce to initialise state, then check a fake one.
  _ <- evalIO $ nextNonce state
  let fake = "not-a-valid-nonce-at-all"
  valid <- evalIO $ checkNonce state fake
  assert (not valid)

-- | Nonces with the same secret are deterministic for the same time interval.
prop_nonceDeterministic :: Property
prop_nonceDeterministic = withTests 3 . property $ do
  let secret = BS.replicate 32 0x42
  state1 <- evalIO $ newNonceState (Just secret) Nothing
  state2 <- evalIO $ newNonceState (Just secret) Nothing
  nonce1 <- evalIO $ nextNonce state1
  nonce2 <- evalIO $ nextNonce state2
  nonce1 === nonce2

-- | Constants have expected values.
prop_constantsValid :: Property
prop_constantsValid = withTests 1 . property $ do
  dpopNonceMaxAgeMs === 180000
  defaultRotationIntervalMs === 60000

tests :: Group
tests = Group "OAuth.Provider.DPoP.Nonce"
  [ ("nonce is non-empty",                prop_nonceNonEmpty)
  , ("nonce round-trips validation",      prop_nonceRoundTrip)
  , ("invalid nonce is rejected",         prop_invalidNonceRejected)
  , ("same secret yields same nonces",    prop_nonceDeterministic)
  , ("constants have expected values",    prop_constantsValid)
  ]
