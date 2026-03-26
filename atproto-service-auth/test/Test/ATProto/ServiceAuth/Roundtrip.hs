module Test.ATProto.ServiceAuth.Roundtrip (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Text      as T
import           Data.Time.Clock.POSIX (getPOSIXTime)

import ATProto.Crypto.EC          (generateKeyPair)
import ATProto.Crypto.Multikey    (encodeMultikey)
import ATProto.Crypto.Types       (Curve (..), PubKey)

import ATProto.ServiceAuth.Create (ServiceJwtParams (..), createServiceJwt)
import ATProto.ServiceAuth.Verify (ServiceJwtPayload (..), ServiceAuthError (..), verifyServiceJwt)
import ATProto.DID                (DidResolver(..), DidDocument (..), ResolveError (..), VerificationMethod (..), CachingDidResolver (..))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A signing-key callback that always returns the given public key.
alwaysReturn :: PubKey -> MockDidResolver
alwaysReturn = MockDidResolver

newtype MockDidResolver =
  MockDidResolver PubKey

instance DidResolver MockDidResolver where
  resolve (MockDidResolver key) did = do
    let
      vmId' =
        did <> "#atproto"
      verifier =
        VerificationMethod vmId' "EcdsaSecp256k1VerificationKey2019" did
            (Just (T.pack (encodeMultikey key)))
    pure . Right $
      DidDocument did []  [verifier] []

instance CachingDidResolver MockDidResolver where
  refreshResolve = resolve

-- | A signing-key callback that returns the first key, then refreshes to the second.
splitReturn :: PubKey -> PubKey -> MockSplitDidResolver
splitReturn = MockSplitDidResolver

data MockSplitDidResolver =
  MockSplitDidResolver PubKey PubKey

instance DidResolver MockSplitDidResolver where
  resolve (MockSplitDidResolver old _) = do
    resolve (MockDidResolver old)

instance CachingDidResolver MockSplitDidResolver where
  refreshResolve (MockSplitDidResolver _ new) = do
    resolve (MockDidResolver new)

data FailResolver = FailResolver

instance CachingDidResolver FailResolver where
  refreshResolve = resolve

instance DidResolver FailResolver where
  resolve FailResolver did =
    pure . Left $
      DidNotFound did

-- | A signing-key callback that always fails.
alwaysFail :: FailResolver
alwaysFail = FailResolver

genIss :: Gen T.Text
genIss = do
  suffix <- Gen.text (Range.linear 5 20) Gen.alphaNum
  return ("did:plc:" <> suffix)

genAud :: Gen T.Text
genAud = do
  suffix <- Gen.text (Range.linear 5 20) Gen.alphaNum
  return ("did:plc:" <> suffix)

genLxm :: Gen T.Text
genLxm = Gen.element
  [ "com.atproto.sync.getRepo"
  , "com.atproto.server.createSession"
  , "app.bsky.feed.getTimeline"
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | A freshly created JWT can be verified successfully.
prop_roundtrip :: Property
prop_roundtrip = property $ do
  iss <- forAll genIss
  aud <- forAll genAud
  mLxm <- forAll $ Gen.maybe genLxm

  (priv, pub) <- evalIO (generateKeyPair P256)

  let params = ServiceJwtParams
        { sjpIss     = iss
        , sjpAud     = aud
        , sjpLxm     = mLxm
        , sjpKeypair = priv
        , sjpExp     = Nothing
        }

  jwt <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt (Just aud) mLxm (alwaysReturn pub))

  case result of
    Left err -> do
      annotate ("unexpected error: " ++ show err)
      failure
    Right payload -> do
      payloadIss payload === iss
      payloadAud payload === aud
      payloadLxm payload === mLxm

-- | Secp256k1 keys also round-trip successfully.
prop_roundtripSecp256k1 :: Property
prop_roundtripSecp256k1 = property $ do
  iss <- forAll genIss
  aud <- forAll genAud

  (priv, pub) <- evalIO (generateKeyPair Secp256k1)

  let params = ServiceJwtParams
        { sjpIss     = iss
        , sjpAud     = aud
        , sjpLxm     = Nothing
        , sjpKeypair = priv
        , sjpExp     = Nothing
        }

  jwt <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt (Just aud) Nothing (alwaysReturn pub))

  case result of
    Left err -> do
      annotate ("unexpected error: " ++ show err)
      failure
    Right payload ->
      payloadIss payload === iss

-- | An expired JWT is rejected.
prop_expired :: Property
prop_expired = property $ do
  (priv, pub) <- evalIO (generateKeyPair P256)
  now <- evalIO $ round <$> getPOSIXTime

  let params = ServiceJwtParams
        { sjpIss     = "did:plc:issuer"
        , sjpAud     = "did:plc:audience"
        , sjpLxm     = Nothing
        , sjpKeypair = priv
        , sjpExp     = Just (now - 100)
        }

  jwt <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt Nothing Nothing (alwaysReturn pub))

  result === Left JwtExpired

-- | A JWT with the wrong audience is rejected.
prop_wrongAudience :: Property
prop_wrongAudience = property $ do
  (priv, pub) <- evalIO (generateKeyPair P256)

  let params = ServiceJwtParams
        { sjpIss     = "did:plc:issuer"
        , sjpAud     = "did:plc:audience"
        , sjpLxm     = Nothing
        , sjpKeypair = priv
        , sjpExp     = Nothing
        }

  jwt <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt (Just "did:plc:wrong") Nothing (alwaysReturn pub))

  result === Left BadJwtAudience

-- | A JWT with the wrong lexicon method is rejected.
prop_wrongLxm :: Property
prop_wrongLxm = property $ do
  (priv, pub) <- evalIO (generateKeyPair P256)

  let params = ServiceJwtParams
        { sjpIss     = "did:plc:issuer"
        , sjpAud     = "did:plc:audience"
        , sjpLxm     = Just "com.atproto.sync.getRepo"
        , sjpKeypair = priv
        , sjpExp     = Nothing
        }

  jwt <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt Nothing (Just "com.atproto.other.method") (alwaysReturn pub))

  result === Left BadJwtLexiconMethod

-- | A JWT with a missing lxm is rejected when one is expected.
prop_missingLxm :: Property
prop_missingLxm = property $ do
  (priv, pub) <- evalIO (generateKeyPair P256)

  let params = ServiceJwtParams
        { sjpIss     = "did:plc:issuer"
        , sjpAud     = "did:plc:audience"
        , sjpLxm     = Nothing
        , sjpKeypair = priv
        , sjpExp     = Nothing
        }

  jwt <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt Nothing (Just "com.atproto.sync.getRepo") (alwaysReturn pub))

  result === Left BadJwtLexiconMethod

-- | A JWT verified with the wrong key is rejected.
prop_wrongKey :: Property
prop_wrongKey = property $ do
  (priv, _)    <- evalIO (generateKeyPair P256)
  (_,    pub2) <- evalIO (generateKeyPair P256)

  let params = ServiceJwtParams
        { sjpIss     = "did:plc:issuer"
        , sjpAud     = "did:plc:audience"
        , sjpLxm     = Nothing
        , sjpKeypair = priv
        , sjpExp     = Nothing
        }

  jwt <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt Nothing Nothing (alwaysReturn pub2))

  result === Left BadJwtSignature

-- | If getSigningKey fails, the error is propagated.
prop_keyResolutionFailure :: Property
prop_keyResolutionFailure = property $ do
  (priv, _) <- evalIO (generateKeyPair P256)

  let params = ServiceJwtParams
        { sjpIss     = "did:plc:issuer"
        , sjpAud     = "did:plc:audience"
        , sjpLxm     = Nothing
        , sjpKeypair = priv
        , sjpExp     = Nothing
        }

  jwt    <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt Nothing Nothing alwaysFail)

  result === Left (BadJwtIss (DidNotFound "did:plc:issuer"))

-- | Skipping aud check (Nothing) allows any audience.
prop_skipAudCheck :: Property
prop_skipAudCheck = property $ do
  (priv, pub) <- evalIO (generateKeyPair P256)

  let params = ServiceJwtParams
        { sjpIss     = "did:plc:issuer"
        , sjpAud     = "did:plc:anyaudience"
        , sjpLxm     = Nothing
        , sjpKeypair = priv
        , sjpExp     = Nothing
        }

  jwt <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt Nothing Nothing (alwaysReturn pub))

  case result of
    Left err -> do
      annotate ("unexpected error: " ++ show err)
      failure
    Right _ -> success

-- | The force-refresh retry succeeds when the initial key is wrong.
prop_forceRefreshRetry :: Property
prop_forceRefreshRetry = property $ do
  (priv, pub)   <- evalIO (generateKeyPair P256)
  (_, wrongPub) <- evalIO (generateKeyPair P256)

  let params = ServiceJwtParams
        { sjpIss     = "did:plc:issuer"
        , sjpAud     = "did:plc:audience"
        , sjpLxm     = Nothing
        , sjpKeypair = priv
        , sjpExp     = Nothing
        }

  jwt    <- evalIO (createServiceJwt params)
  result <- evalIO (verifyServiceJwt jwt Nothing Nothing (splitReturn wrongPub pub))

  case result of
    Left err -> do
      annotate ("unexpected error: " ++ show err)
      failure
    Right payload ->
      payloadIss payload === "did:plc:issuer"

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ServiceAuth.Roundtrip"
  [ ("roundtrip P256",                      prop_roundtrip)
  , ("roundtrip Secp256k1",                 prop_roundtripSecp256k1)
  , ("expired JWT rejected",                prop_expired)
  , ("wrong audience rejected",             prop_wrongAudience)
  , ("wrong lxm rejected",                  prop_wrongLxm)
  , ("missing lxm rejected",                prop_missingLxm)
  , ("wrong key rejected",                  prop_wrongKey)
  , ("key resolution failure propagated",   prop_keyResolutionFailure)
  , ("skip aud check allows any audience",  prop_skipAudCheck)
  , ("force-refresh retry succeeds",        prop_forceRefreshRetry)
  ]
