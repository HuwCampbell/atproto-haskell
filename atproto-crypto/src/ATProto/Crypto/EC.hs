-- | ECDSA signing and verification for the AT Protocol.
--
-- Security properties enforced here:
--
-- 1. __Low-S signatures__: generated signatures always satisfy @s <= n\/2@,
--    preventing the malleability attack where @(r, n-s)@ is a second valid
--    signature over the same message.
--
-- 2. __Compact format__: signatures are exactly 64 bytes (@r || s@, each
--    32-byte big-endian), not DER-encoded.
--
-- 3. __SHA-256 digest__: the message is hashed with SHA-256 before signing
--    (handled internally by 'Crypto.PubKey.ECC.ECDSA.sign').
--
-- 4. __OS entropy__: key generation and signing use the OS CSPRNG via
--    'Crypto.Random.MonadRandom', not a user-supplied seed.
--
-- 5. __Point validation__: public keys are validated to lie on the curve
--    during decompression; out-of-range scalars are rejected on import.
module ATProto.Crypto.EC
  ( -- * Key management
    generateKeyPair
  , importPrivKey
  , derivePublicKey
    -- * Signing and verification
  , sign
  , verify
  ) where

import qualified Data.ByteString                    as BS
import qualified Crypto.PubKey.ECC.ECDSA            as ECDSA
import qualified Crypto.PubKey.ECC.Generate         as ECGen
import qualified Crypto.PubKey.ECC.Types            as ECC
import qualified Crypto.PubKey.ECC.Prim             as ECPrim
import           Crypto.Hash.Algorithms             (SHA256 (..))

import ATProto.Crypto.Types

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Generate a fresh keypair using the OS CSPRNG.
generateKeyPair :: Curve -> IO (PrivKey, PubKey)
generateKeyPair curve = do
  let ecCurve = toCryptonCurve curve
  (pub, priv) <- ECGen.generate ecCurve
  let privBytes = integerToBS32 (ECDSA.private_d priv)
      pubBytes  = compressPoint (ECDSA.public_q pub)
  return (PrivKey curve privBytes, PubKey curve pubBytes)

-- | Import a private key from 32 raw bytes.
--
-- Returns 'Left' if the byte string is not 32 bytes or the value is
-- outside the valid range @[1, n)@ for the curve.
importPrivKey :: Curve -> BS.ByteString -> Either String PrivKey
importPrivKey curve bs
  | BS.length bs /= 32    = Left "Private key must be exactly 32 bytes"
  | d <= 0                = Left "Private key scalar must be positive"
  | d >= curveOrder curve = Left "Private key scalar is not in [1, n)"
  | otherwise             = Right (PrivKey curve bs)
  where
    d = bsToInteger bs

-- | Derive the compressed public key from a private key.
derivePublicKey :: PrivKey -> PubKey
derivePublicKey (PrivKey curve privBytes) =
  let ecCurve = toCryptonCurve curve
      n       = curveOrder curve
      g       = ECC.ecc_g (ECC.common_curve ecCurve)
      pt      = ECPrim.pointMul ecCurve (bsToInteger privBytes `mod` n) g
  in PubKey curve (compressPoint pt)

-- | Sign a message with a private key.
--
-- SHA-256 is applied to @msg@ internally.  The resulting signature is in
-- compact format (64 bytes) and always has low-S.
--
-- This operation uses the OS CSPRNG for the ECDSA nonce @k@.
sign :: PrivKey -> BS.ByteString -> IO Signature
sign (PrivKey curve privBytes) msg = do
  let ecCurve = toCryptonCurve curve
      n       = curveOrder curve
      privKey = ECDSA.PrivateKey ecCurve (bsToInteger privBytes)
  ECDSA.Signature r rawS <- ECDSA.sign privKey SHA256 msg
  -- Normalise to low-S: if s > n/2, replace s with n-s.
  let s = if rawS > n `div` 2 then n - rawS else rawS
  return $ Signature (integerToBS32 r <> integerToBS32 s)

-- | Verify a signature against a message and public key.
--
-- In 'Strict' mode (recommended), signatures with @s > n\/2@ are
-- rejected outright to prevent malleability.  In 'Lax' mode both
-- low-S and high-S are accepted (for interoperability with legacy data).
--
-- Returns 'False' on any parse or curve error, so it is safe to call
-- with untrusted inputs.
verify :: SigStrictness -> PubKey -> BS.ByteString -> Signature -> Bool
verify strictness (PubKey curve pubBytes) msg (Signature sigBytes)
  | BS.length sigBytes /= 64 = False
  | otherwise =
      case decompressPoint curve pubBytes of
        Left  _  -> False
        Right pt ->
          let ecCurve = toCryptonCurve curve
              n       = curveOrder curve
              pubKey  = ECDSA.PublicKey ecCurve pt
              r       = bsToInteger (BS.take 32 sigBytes)
              s       = bsToInteger (BS.drop 32 sigBytes)
              sig     = ECDSA.Signature r s
          in case strictness of
               Strict -> s > 0 && s <= n `div` 2
                           && ECDSA.verify SHA256 pubKey sig msg
               Lax    -> s > 0 && s < n
                           && ECDSA.verify SHA256 pubKey sig msg

-- ---------------------------------------------------------------------------
-- Curve helpers
-- ---------------------------------------------------------------------------

-- | Convert our 'Curve' to crypton's internal 'ECC.Curve'.
toCryptonCurve :: Curve -> ECC.Curve
toCryptonCurve P256      = ECC.getCurveByName ECC.SEC_p256r1
toCryptonCurve Secp256k1 = ECC.getCurveByName ECC.SEC_p256k1

-- | The curve order @n@ (the number of points in the group).
curveOrder :: Curve -> Integer
curveOrder = ECC.ecc_n . ECC.common_curve . toCryptonCurve

-- | Compress an affine point to the 33-byte SEC1 compressed encoding.
--
-- The prefix byte is @0x02@ for even y and @0x03@ for odd y.
compressPoint :: ECC.Point -> BS.ByteString
compressPoint ECC.PointO      = BS.empty   -- should never occur for valid keys
compressPoint (ECC.Point x y) =
  let prefix = if even y then 0x02 else 0x03
  in BS.cons prefix (integerToBS32 x)

-- | Decompress a 33-byte SEC1 compressed point.
--
-- Both P-256 and secp256k1 have prime fields with @p ≡ 3 (mod 4)@, so
-- the modular square root is @y = rhs^((p+1)\/4) mod p@.
--
-- Returns 'Left' if the bytes are malformed or the point is not on the curve.
decompressPoint :: Curve -> BS.ByteString -> Either String ECC.Point
decompressPoint curve bs
  | BS.length bs /= 33       = Left "Compressed public key must be 33 bytes"
  | pre /= 0x02 && pre /= 0x03 = Left "Invalid compressed point prefix (expected 0x02 or 0x03)"
  | x >= p                   = Left "X coordinate is not in the field"
  | y2 /= rhs                = Left "Point is not on the curve"
  | not (ECPrim.isPointValid ecCurve pt)
                             = Left "Point failed curve validation"
  | otherwise                = Right pt
  where
    pre     = BS.head bs
    x       = bsToInteger (BS.tail bs)
    (p, a, b) = primeCurveParams curve
    ecCurve   = toCryptonCurve curve
    -- y² = x³ + ax + b  (mod p)
    rhs     = (powMod x 3 p + a * x + b) `mod` p
    -- Square root: y = rhs^((p+1)/4) mod p  (valid since p ≡ 3 mod 4)
    y0      = powMod rhs ((p + 1) `div` 4) p
    -- Choose the root whose parity matches the prefix.
    -- prefix 0x02 → even y, 0x03 → odd y.
    y       = if even y0 == (pre == 0x02) then y0 else p - y0
    y2      = (y * y) `mod` p
    pt      = ECC.Point x y

-- | Prime field parameters @(p, a, b)@ for the Weierstrass equation
-- @y² = x³ + ax + b (mod p)@.
primeCurveParams :: Curve -> (Integer, Integer, Integer)
primeCurveParams P256 =
  -- p = 2^256 - 2^224 + 2^192 + 2^96 - 1
  ( 0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
  -- a = -3 mod p (stored as positive representative)
  , 0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC
  , 0x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B
  )
primeCurveParams Secp256k1 =
  -- p = 2^256 - 2^32 - 977
  ( 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
  , 0   -- a = 0
  , 7   -- b = 7
  )

-- ---------------------------------------------------------------------------
-- Serialisation helpers
-- ---------------------------------------------------------------------------

-- | Decode a big-endian 'BS.ByteString' to a non-negative 'Integer'.
bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl (\acc b -> acc * 256 + fromIntegral b) 0

-- | Encode a non-negative 'Integer' as a big-endian 32-byte 'BS.ByteString'.
--
-- Pads with leading zero bytes; the caller is responsible for ensuring the
-- value fits in 32 bytes (true for all ECDSA r, s, x values on these curves).
integerToBS32 :: Integer -> BS.ByteString
integerToBS32 n = BS.pack (go (32 :: Int) n [])
  where
    go 0 _ acc = acc
    go k x acc = go (k - 1) (x `div` 256) (fromIntegral (x `mod` 256) : acc)

-- | Fast (non-constant-time) modular exponentiation.
--
-- Used only for public point decompression (no secret involved).
powMod :: Integer -> Integer -> Integer -> Integer
powMod base0 exp0 m = go base0 exp0 1
  where
    go _    0 acc = acc
    go b    e acc =
      let acc' = if odd e then (acc * b) `mod` m else acc
          b'   = (b * b) `mod` m
          e'   = e `div` 2
      in go b' e' acc'
