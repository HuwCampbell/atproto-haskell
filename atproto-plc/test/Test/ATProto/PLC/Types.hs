-- | Tests for PLC operation types and DID derivation.
module Test.ATProto.PLC.Types (tests) where

import Hedgehog
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import ATProto.Crypto        (generateKeyPair, Curve (..))
import ATProto.Crypto.DidKey (pubKeyToDidKey)
import ATProto.PLC.Types
import ATProto.Syntax.DID    (unDID)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

sampleUop :: T.Text -> T.Text -> UnsignedPlcOp
sampleUop rotKeyDid signKeyDid = UnsignedPlcOp
  { uopRotationKeys        = [rotKeyDid]
  , uopVerificationMethods = Map.singleton "atproto" signKeyDid
  , uopAlsoKnownAs         = ["at://alice.example.com"]
  , uopServices            = Map.singleton "atproto_pds"
      PlcService
        { plcServiceType     = "AtprotoPersonalDataServer"
        , plcServiceEndpoint = "https://pds.example.com"
        }
  , uopPrev = Nothing
  }

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | 'plcOpDid' should always return a well-formed did:plc: identifier.
prop_didPlcPrefix :: Property
prop_didPlcPrefix = withTests 20 . property $ do
  (_, rotPub)   <- evalIO $ generateKeyPair Secp256k1
  (_, signPub)  <- evalIO $ generateKeyPair Secp256k1
  let rotKeyDid  = T.pack (pubKeyToDidKey rotPub)
      signKeyDid = T.pack (pubKeyToDidKey signPub)
      uop        = sampleUop rotKeyDid signKeyDid
  did <- evalEither (plcOpDid uop)
  -- The derived DID should start with "did:plc:"
  annotate (T.unpack (unDID did))
  assert (T.isPrefixOf "did:plc:" (unDID did))

-- | Two genesis operations with different keys must produce different DIDs.
prop_differentKeysDifferentDids :: Property
prop_differentKeysDifferentDids = withTests 20 . property $ do
  (_, rotPub1)   <- evalIO $ generateKeyPair Secp256k1
  (_, rotPub2)   <- evalIO $ generateKeyPair Secp256k1
  (_, signPub1)  <- evalIO $ generateKeyPair P256
  (_, signPub2)  <- evalIO $ generateKeyPair P256
  let rotKeyDid1  = T.pack (pubKeyToDidKey rotPub1)
      rotKeyDid2  = T.pack (pubKeyToDidKey rotPub2)
      signKeyDid1 = T.pack (pubKeyToDidKey signPub1)
      signKeyDid2 = T.pack (pubKeyToDidKey signPub2)
      uop1 = sampleUop rotKeyDid1 signKeyDid1
      uop2 = sampleUop rotKeyDid2 signKeyDid2
  did1 <- evalEither (plcOpDid uop1)
  did2 <- evalEither (plcOpDid uop2)
  did1 /== did2

-- | 'plcOpDid' is deterministic: the same unsigned op always gives the same DID.
prop_didIsDeterministic :: Property
prop_didIsDeterministic = withTests 20 . property $ do
  (_, rotPub)   <- evalIO $ generateKeyPair Secp256k1
  (_, signPub)  <- evalIO $ generateKeyPair P256
  let rotKeyDid  = T.pack (pubKeyToDidKey rotPub)
      signKeyDid = T.pack (pubKeyToDidKey signPub)
      uop        = sampleUop rotKeyDid signKeyDid
  did1 <- evalEither (plcOpDid uop)
  did2 <- evalEither (plcOpDid uop)
  did1 === did2

-- | Signing does not change the DID derived from the unsigned operation.
prop_signingPreservesDid :: Property
prop_signingPreservesDid = withTests 10 . property $ do
  (rotPriv, rotPub)   <- evalIO $ generateKeyPair Secp256k1
  (_, signPub)        <- evalIO $ generateKeyPair P256
  let rotKeyDid  = T.pack (pubKeyToDidKey rotPub)
      signKeyDid = T.pack (pubKeyToDidKey signPub)
      uop        = sampleUop rotKeyDid signKeyDid
  did   <- evalEither (plcOpDid uop)
  op    <- evalIO (signPlcOp rotPriv uop)
  did2  <- evalEither (plcOpDid (popUnsigned op))
  did === did2

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.PLC.Types"
  [ ("did:plc: prefix",              prop_didPlcPrefix)
  , ("different keys → different DIDs", prop_differentKeysDifferentDids)
  , ("DID derivation is deterministic", prop_didIsDeterministic)
  , ("signing preserves DID",           prop_signingPreservesDid)
  ]
