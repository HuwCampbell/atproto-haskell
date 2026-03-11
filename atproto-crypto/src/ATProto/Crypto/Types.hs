-- | AT Protocol cryptographic key types.
--
-- Port of \@atproto\/crypto – key representations and signing / verification
-- operations for the two curves used in the AT Protocol: P-256 (NIST) and
-- secp256k1.
module ATProto.Crypto.Types
  ( -- * Key types
    Curve (..)
  , KeyEncoding (..)
  , PrivKey (..)
  , PubKey (..)
    -- * Signature
  , Signature (..)
  ) where

import qualified Data.ByteString as BS

-- | The elliptic curves used by the AT Protocol.
data Curve
  = P256      -- ^ NIST P-256 (also called secp256r1 / prime256v1)
  | Secp256k1 -- ^ Bitcoin-style secp256k1
  deriving (Eq, Ord, Show)

-- | Serialisation format for public keys.
data KeyEncoding
  = CompressedHex   -- ^ Compressed point, hex-encoded
  | Multibase       -- ^ Multibase-encoded multikey (base58btc prefix)
  deriving (Eq, Ord, Show)

-- | An opaque private key.
data PrivKey = PrivKey
  { privKeyCurve :: Curve
  , privKeyBytes :: BS.ByteString
  } deriving (Eq, Show)

-- | An opaque public key.
data PubKey = PubKey
  { pubKeyCurve :: Curve
  , pubKeyBytes :: BS.ByteString
    -- ^ Compressed-point encoding.
  } deriving (Eq, Show)

-- | A raw DER or compact signature.
newtype Signature = Signature { unSignature :: BS.ByteString }
  deriving (Eq, Show)
