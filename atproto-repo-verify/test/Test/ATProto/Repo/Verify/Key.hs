module Test.ATProto.Repo.Verify.Key (tests) where

import Hedgehog

import ATProto.DID.Document
import ATProto.Repo.Verify.Key

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A minimal DID document with an #atproto verification method.
-- The multikey encodes the P-256 generator point (private key scalar = 1).
fixtureDocWithAtproto :: DidDocument
fixtureDocWithAtproto = DidDocument
  { didDocId                  = "did:plc:test123"
  , didDocAlsoKnownAs         = []
  , didDocVerificationMethods =
      [ VerificationMethod
          { vmId                 = "did:plc:test123#atproto"
          , vmType               = "Multikey"
          , vmController         = "did:plc:test123"
          , vmPublicKeyMultibase = Just "zDnaepsL7AXenJkVYdkh5KuKsSU7Ykh7kyXaLLU7auN9FWSiZ"
          }
      ]
  , didDocServices            = []
  }

-- | A DID document with no #atproto verification method.
fixtureDocWithoutAtproto :: DidDocument
fixtureDocWithoutAtproto = DidDocument
  { didDocId                  = "did:plc:test123"
  , didDocAlsoKnownAs         = []
  , didDocVerificationMethods =
      [ VerificationMethod
          { vmId                 = "did:plc:test123#other"
          , vmType               = "Multikey"
          , vmController         = "did:plc:test123"
          , vmPublicKeyMultibase = Just "zDnaepsL7AXenJkVYdkh5KuKsSU7Ykh7kyXaLLU7auN9FWSiZ"
          }
      ]
  , didDocServices            = []
  }

-- | A DID document with an #atproto method but no publicKeyMultibase.
fixtureDocNoKey :: DidDocument
fixtureDocNoKey = DidDocument
  { didDocId                  = "did:plc:test123"
  , didDocAlsoKnownAs         = []
  , didDocVerificationMethods =
      [ VerificationMethod
          { vmId                 = "did:plc:test123#atproto"
          , vmType               = "Multikey"
          , vmController         = "did:plc:test123"
          , vmPublicKeyMultibase = Nothing
          }
      ]
  , didDocServices            = []
  }

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Extracting the key from a well-formed document succeeds.
prop_resolveOk :: Property
prop_resolveOk = property $ do
  case resolveAtprotoKey fixtureDocWithAtproto of
    Left err -> do
      annotate (show err)
      failure
    Right _  -> success

-- | Missing #atproto method yields a Left error.
prop_noAtprotoMethod :: Property
prop_noAtprotoMethod = property $ do
  case resolveAtprotoKey fixtureDocWithoutAtproto of
    Left _  -> success
    Right _ -> failure

-- | Missing publicKeyMultibase yields a Left error.
prop_noMultibase :: Property
prop_noMultibase = property $ do
  case resolveAtprotoKey fixtureDocNoKey of
    Left _  -> success
    Right _ -> failure

-- | An empty document yields a Left error.
prop_emptyDoc :: Property
prop_emptyDoc = property $ do
  let emptyDoc = DidDocument "did:plc:x" [] [] []
  case resolveAtprotoKey emptyDoc of
    Left _  -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.Repo.Verify.Key"
  [ ("resolve key from well-formed document",  prop_resolveOk)
  , ("no #atproto method yields error",        prop_noAtprotoMethod)
  , ("no publicKeyMultibase yields error",     prop_noMultibase)
  , ("empty document yields error",            prop_emptyDoc)
  ]
