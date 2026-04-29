-- | Types for PLC (Placeholder) directory operations.
--
-- A @did:plc@ identifier is derived from a signed genesis operation:
--
-- 1. Build an 'UnsignedPlcOp' with rotation keys, verification methods,
--    aliases, and services.
-- 2. Encode it as DAG-CBOR with 'encodePlcOpForSigning'.
-- 3. Sign the encoding with a rotation key.
-- 4. Attach the signature (base64url-encoded) to form a 'PlcOp'.
-- 5. Derive the DID with 'plcOpDid'.
-- 6. Submit via 'ATProto.PLC.Client.submitPlcOp'.
--
-- = PLC specification
--
-- <https://web.plc.directory/spec/v0.1/did-plc>
module ATProto.PLC.Types
  ( -- * PLC service descriptor
    PlcService (..)
    -- * Unsigned operation
  , UnsignedPlcOp (..)
    -- * Signed operation
  , PlcOp (..)
    -- * Signing
  , signPlcOp
    -- * DID derivation
  , plcOpDid
    -- * DAG-CBOR encoding helper
  , encodePlcOpForSigning
  ) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteArray         as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.Char              as Char
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import qualified Codec.CBOR.Encoding    as E
import qualified Codec.CBOR.Write       as W
import           Crypto.Hash            (hashWith, SHA256 (..))

import           ATProto.Crypto.EC      (sign)
import           ATProto.Crypto.Types   (PrivKey, Signature (..))
import           ATProto.Syntax.DID     (DID, parseDID)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A service entry in a PLC operation.
data PlcService = PlcService
  { plcServiceType     :: !T.Text
    -- ^ Service type, e.g. @\"AtprotoPersonalDataServer\"@.
  , plcServiceEndpoint :: !T.Text
    -- ^ Service endpoint URL, e.g. @\"https://pds.example.com\"@.
  } deriving (Eq, Show)

-- | A PLC operation without a signature.
--
-- Build one of these, encode it for signing, sign it, and then attach
-- the signature to get a 'PlcOp'.
data UnsignedPlcOp = UnsignedPlcOp
  { uopRotationKeys          :: ![T.Text]
    -- ^ @did:key@ URIs of the rotation keys, in priority order.
    -- The first key in this list is the highest-priority rotation key.
  , uopVerificationMethods   :: !(Map.Map T.Text T.Text)
    -- ^ Map of verification method ID to @did:key@ URI.
    -- For a standard Bluesky PDS account, this is
    -- @{\"atproto\": \"did:key:z...\"}@.
  , uopAlsoKnownAs           :: ![T.Text]
    -- ^ @at://\<handle\>@ aliases for this DID, e.g.
    -- @[\"at://alice.example.com\"]@.
  , uopServices              :: !(Map.Map T.Text PlcService)
    -- ^ Services hosted under this DID.  For a PDS account, this is
    -- @{\"atproto_pds\": PlcService{...}}@.
  , uopPrev                  :: !(Maybe T.Text)
    -- ^ CID of the previous operation, or 'Nothing' for a genesis op.
  } deriving (Eq, Show)

-- | A signed PLC operation ready for submission to the PLC directory.
data PlcOp = PlcOp
  { popUnsigned :: !UnsignedPlcOp
    -- ^ The unsigned content of this operation.
  , popSig      :: !T.Text
    -- ^ Base64url-encoded signature over the DAG-CBOR encoding of the
    -- unsigned operation.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Signing
-- ---------------------------------------------------------------------------

-- | Sign an 'UnsignedPlcOp' with a rotation key, producing a 'PlcOp'.
--
-- The signature is computed over the DAG-CBOR encoding of the unsigned
-- operation (see 'encodePlcOpForSigning').
signPlcOp :: PrivKey -> UnsignedPlcOp -> IO PlcOp
signPlcOp key uop = do
  let payload = encodePlcOpForSigning uop
  Signature sigBytes <- sign key payload
  let sig = TE.decodeUtf8 (BAE.convertToBase BAE.Base64URLUnpadded sigBytes :: BS.ByteString)
  return PlcOp { popUnsigned = uop, popSig = sig }

-- ---------------------------------------------------------------------------
-- DID derivation
-- ---------------------------------------------------------------------------

-- | Derive the @did:plc:@ identifier for a genesis operation.
--
-- The DID is @did:plc:\<base32lower(sha256(cbor)[0:15])\>@:
--
-- 1. Encode the __unsigned__ operation as DAG-CBOR.
-- 2. Hash with SHA-256 (32 bytes).
-- 3. Take the first 15 bytes of the hash.
-- 4. Encode as base32 (lowercase, no padding).
-- 5. Prepend @did:plc:@.
--
-- 15 bytes was chosen by the PLC specification to yield a 24-character
-- base32 identifier, balancing brevity with collision resistance.
--
-- Returns 'Left' only if the resulting string fails DID validation
-- (which should never happen for a correctly-formed operation).
plcOpDid :: UnsignedPlcOp -> Either String DID
plcOpDid uop =
  let cbor     = encodePlcOpForSigning uop
      digest   = hashWith SHA256 cbor
      hashBS   = BS.take 15 (BA.convert digest :: BS.ByteString)
      b32      = T.map Char.toLower
               . TE.decodeUtf8
               $ (BAE.convertToBase BAE.Base32 hashBS :: BS.ByteString)
      -- Drop any '=' padding characters that the Base32 encoder adds.
      b32clean = T.dropWhileEnd (== '=') b32
      didText  = "did:plc:" <> b32clean
  in  parseDID didText

-- ---------------------------------------------------------------------------
-- DAG-CBOR encoding
-- ---------------------------------------------------------------------------

-- | Encode an 'UnsignedPlcOp' as DAG-CBOR for signing or DID derivation.
--
-- Keys are serialised in canonical DAG-CBOR order (by CBOR-encoded key
-- length, then lexicographically):
--
-- @prev@ (4), @type@ (4), @services@ (8), @alsoKnownAs@ (11),
-- @rotationKeys@ (12), @verificationMethods@ (19)
encodePlcOpForSigning :: UnsignedPlcOp -> BS.ByteString
encodePlcOpForSigning uop =
  BL.toStrict . W.toLazyByteString $
       E.encodeMapLen 6
    -- "prev" (4 bytes) before "type" (4 bytes) lexicographically
    <> E.encodeString "prev"
    <> encodePrev (uopPrev uop)
    <> E.encodeString "type"
    <> E.encodeString "plc_operation"
    -- "services" (8 bytes)
    <> E.encodeString "services"
    <> encodeServices (uopServices uop)
    -- "alsoKnownAs" (11 bytes)
    <> E.encodeString "alsoKnownAs"
    <> encodeTextList (uopAlsoKnownAs uop)
    -- "rotationKeys" (12 bytes)
    <> E.encodeString "rotationKeys"
    <> encodeTextList (uopRotationKeys uop)
    -- "verificationMethods" (19 bytes)
    <> E.encodeString "verificationMethods"
    <> encodeTextMap (uopVerificationMethods uop)

-- | Encode @prev@: null for a genesis op, or a text CID string otherwise.
encodePrev :: Maybe T.Text -> E.Encoding
encodePrev Nothing    = E.encodeNull
encodePrev (Just cid) = E.encodeString cid

-- | Encode a list of text values.
encodeTextList :: [T.Text] -> E.Encoding
encodeTextList xs =
  E.encodeListLen (fromIntegral (length xs))
    <> foldMap E.encodeString xs

-- | Encode a @Text -> Text@ map in lexicographic key order.
encodeTextMap :: Map.Map T.Text T.Text -> E.Encoding
encodeTextMap m =
  E.encodeMapLen (fromIntegral (Map.size m))
    <> Map.foldlWithKey'
         (\acc k v -> acc <> E.encodeString k <> E.encodeString v)
         mempty
         m

-- | Encode the @services@ map.
--
-- Each service is a 2-element map @{\"type\": ..., \"endpoint\": ...}@.
-- Keys within the service map are in canonical order: \"type\" (4) before
-- \"endpoint\" (8).
encodeServices :: Map.Map T.Text PlcService -> E.Encoding
encodeServices m =
  E.encodeMapLen (fromIntegral (Map.size m))
    <> Map.foldlWithKey'
         (\acc k svc -> acc
            <> E.encodeString k
            <> E.encodeMapLen 2
            <> E.encodeString "type"
            <> E.encodeString (plcServiceType svc)
            <> E.encodeString "endpoint"
            <> E.encodeString (plcServiceEndpoint svc)
         )
         mempty
         m
