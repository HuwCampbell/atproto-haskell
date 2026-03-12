-- | Core types for the AT Protocol cryptographic layer.
--
-- The AT Protocol uses two elliptic curves:
--
--  * P-256  (NIST secp256r1) – JWT algorithm ES256
--  * secp256k1 (Bitcoin curve) – JWT algorithm ES256K
--
-- All signatures are in __compact__ (r||s) format: exactly 64 bytes with r
-- and s each encoded as a 32-byte big-endian unsigned integer.
--
-- To prevent signature malleability, the AT Protocol requires __low-S__
-- signatures: @s <= n/2@ where @n@ is the curve order.  Use 'Strict' mode
-- for verification unless you are dealing with legacy data.
module ATProto.Crypto.Types
  ( -- * Curves
    Curve (..)
  , jwtAlg
    -- * Signature strictness
  , SigStrictness (..)
    -- * Keys
  , PrivKey (..)
  , PubKey (..)
    -- * Signatures
  , Signature (..)
  ) where

import qualified Data.ByteString as BS

-- | The two elliptic curves used by the AT Protocol.
data Curve
  = P256
    -- ^ NIST P-256 (secp256r1 / prime256v1).  JWT algorithm: ES256.
  | Secp256k1
    -- ^ Bitcoin-style secp256k1.  JWT algorithm: ES256K.
  deriving (Eq, Ord, Show)

-- | The JWT algorithm identifier for a curve.
jwtAlg :: Curve -> String
jwtAlg P256      = "ES256"
jwtAlg Secp256k1 = "ES256K"

-- | Whether signature verification should enforce low-S.
--
-- ECDSA signatures are malleable: for any valid @(r, s)@, the pair
-- @(r, n-s)@ is also a valid signature over the same message.  The AT
-- Protocol prevents this by requiring @s <= n/2@ (low-S).
--
-- * Use 'Strict' for all new code – rejects high-S signatures.
-- * Use 'Lax' only for compatibility with legacy data that may carry
--   high-S signatures.
data SigStrictness
  = Strict  -- ^ Reject high-S signatures.
  | Lax     -- ^ Accept both low-S and high-S.
  deriving (Eq, Ord, Show)

-- | An opaque private key (32-byte big-endian scalar).
data PrivKey = PrivKey
  { privKeyCurve :: Curve
  , privKeyBytes :: BS.ByteString
    -- ^ Raw 32-byte scalar in @[1, n)@ for the curve.
  } deriving (Eq, Show)

-- | An opaque public key (33-byte compressed point encoding).
--
-- The first byte is @0x02@ (even y) or @0x03@ (odd y); the remaining
-- 32 bytes are the x-coordinate in big-endian.
data PubKey = PubKey
  { pubKeyCurve :: Curve
  , pubKeyBytes :: BS.ByteString
    -- ^ Compressed point: 1 prefix byte + 32-byte x-coordinate.
  } deriving (Eq, Show)

-- | A compact ECDSA signature (exactly 64 bytes).
--
-- Bytes 0–31 are @r@; bytes 32–63 are @s@, both big-endian unsigned.
-- Signatures produced by 'ATProto.Crypto.EC.sign' always satisfy
-- @s <= n/2@ (low-S).
newtype Signature = Signature { unSignature :: BS.ByteString }
  deriving (Eq, Show)
