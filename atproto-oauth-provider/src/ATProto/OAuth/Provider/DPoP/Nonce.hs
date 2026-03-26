-- | HMAC-based rotating DPoP nonces.
--
-- Ported from the upstream TypeScript reference implementation
-- (@atproto\/oauth-provider\/src\/dpop\/dpop-nonce.ts@).
--
-- DPoP nonces are used to prevent replay of DPoP proofs.  The server
-- generates nonces using HMAC-SHA256 over a monotonically increasing
-- counter that rotates at a fixed interval.  At any point in time,
-- three nonces are valid: the previous, current, and next rotation.
--
-- = Security properties
--
-- * Nonces are deterministic given a secret and counter, allowing
--   multiple server instances sharing the same secret to accept each
--   other's nonces without coordination.
-- * The secret should be 32 bytes of cryptographically random data.
-- * The rotation interval should be at most @DPOP_NONCE_MAX_AGE \/ 3@
--   (60 seconds with 3 minute max age).
--
-- = Usage
--
-- @
-- import ATProto.OAuth.Provider.DPoP.Nonce
--
-- main :: IO ()
-- main = do
--   state <- newNonceState Nothing Nothing
--   nonce <- nextNonce state
--   valid <- checkNonce state nonce
--   putStrLn (\"Valid: \" ++ show valid)
-- @
module ATProto.OAuth.Provider.DPoP.Nonce
  ( -- * State
    NonceState
  , newNonceState
    -- * Operations
  , nextNonce
  , checkNonce
    -- * Constants
  , dpopNonceMaxAgeMs
  , defaultRotationIntervalMs
  ) where

import           Data.IORef          (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteArray      as BA
import           Data.ByteArray.Encoding (Base (..), convertToBase)
import           Data.Bits           (shiftR)
import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import           Data.Word           (Word8)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Crypto.MAC.HMAC     as HMAC
import           Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.Random       as Random

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Maximum DPoP nonce age in milliseconds (3 minutes).
dpopNonceMaxAgeMs :: Int
dpopNonceMaxAgeMs = 180000

-- | Default rotation interval in milliseconds (60 seconds).
-- This is @dpopNonceMaxAgeMs / 3@.
defaultRotationIntervalMs :: Int
defaultRotationIntervalMs = dpopNonceMaxAgeMs `div` 3

-- | Secret byte length (32 bytes = 256 bits).
secretByteLength :: Int
secretByteLength = 32

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

-- | Mutable state for DPoP nonce generation and validation.
data NonceState = NonceState
  { nsRotationInterval :: !Int
    -- ^ Rotation interval in milliseconds.
  , nsSecret           :: !BS.ByteString
    -- ^ 32-byte HMAC secret.
  , nsRef              :: !(IORef NonceCache)
    -- ^ Cached nonces and current counter.
  }

-- | Internal cache of the three valid nonces.
data NonceCache = NonceCache
  { ncCounter :: !Int
  , ncPrev    :: !T.Text
  , ncNow     :: !T.Text
  , ncNext    :: !T.Text
  }

-- | Create a new 'NonceState'.
--
-- If no secret is provided, 32 random bytes are generated.
-- If no rotation interval is provided, 'defaultRotationIntervalMs' is used.
newNonceState
  :: Maybe BS.ByteString
  -- ^ Optional 32-byte secret.  Pass 'Nothing' to generate a random one.
  -> Maybe Int
  -- ^ Optional rotation interval in milliseconds.
  -> IO NonceState
newNonceState mSecret mInterval = do
  secret <- case mSecret of
    Just s  | BS.length s == secretByteLength -> return s
    Just _  -> fail "DPoP nonce secret must be exactly 32 bytes"
    Nothing -> Random.getRandomBytes secretByteLength
  let interval = maybe defaultRotationIntervalMs id mInterval
  counter <- currentCounter interval
  let prev = computeNonce secret (counter - 1)
      now  = computeNonce secret counter
      nxt  = computeNonce secret (counter + 1)
  ref <- newIORef NonceCache
    { ncCounter = counter
    , ncPrev    = prev
    , ncNow     = now
    , ncNext    = nxt
    }
  return NonceState
    { nsRotationInterval = interval
    , nsSecret           = secret
    , nsRef              = ref
    }

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

-- | Get the next DPoP nonce to send to a client.
--
-- Rotates the nonce if the current interval has elapsed.
nextNonce :: NonceState -> IO T.Text
nextNonce state = do
  cache <- rotate state
  return (ncNext cache)

-- | Check whether a nonce is valid.
--
-- A nonce is valid if it matches the previous, current, or next rotation.
checkNonce :: NonceState -> T.Text -> IO Bool
checkNonce state nonce = do
  cache <- rotate state
  return (   nonce == ncNext cache
          || nonce == ncNow  cache
          || nonce == ncPrev cache )

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Get the current counter value (number of full rotations since epoch).
currentCounter :: Int -> IO Int
currentCounter interval = do
  now <- getPOSIXTime
  let ms = floor (now * 1000) :: Int
  return (ms `div` interval)

-- | Rotate the nonce cache if the counter has advanced.
rotate :: NonceState -> IO NonceCache
rotate state = do
  cache <- readIORef (nsRef state)
  counter <- currentCounter (nsRotationInterval state)
  let diff = counter - ncCounter cache
  if diff == 0
    then return cache
    else do
      let secret = nsSecret state
          cache' = case diff of
            1 -> NonceCache
              { ncCounter = counter
              , ncPrev = ncNow cache
              , ncNow  = ncNext cache
              , ncNext = computeNonce secret (counter + 1)
              }
            2 -> NonceCache
              { ncCounter = counter
              , ncPrev = ncNext cache
              , ncNow  = computeNonce secret counter
              , ncNext = computeNonce secret (counter + 1)
              }
            _ -> NonceCache
              { ncCounter = counter
              , ncPrev = computeNonce secret (counter - 1)
              , ncNow  = computeNonce secret counter
              , ncNext = computeNonce secret (counter + 1)
              }
      writeIORef (nsRef state) cache'
      return cache'

-- | Compute an HMAC-SHA256 nonce for a given counter value.
--
-- Matches the upstream: @HMAC-SHA256(secret, counter_as_64bit_be)@
-- encoded as base64url-unpadded.
computeNonce :: BS.ByteString -> Int -> T.Text
computeNonce secret counter =
  let input = numTo64Bits counter
      hmacResult = HMAC.hmac secret input :: HMAC.HMAC SHA256
      digest = BA.convert hmacResult :: BS.ByteString
  in  TE.decodeUtf8 (convertToBase Base64URLUnpadded digest)

-- | Encode an 'Int' as a big-endian 8-byte (64-bit) 'BS.ByteString'.
--
-- Matches the upstream @numTo64bits@ function.
numTo64Bits :: Int -> BS.ByteString
numTo64Bits n =
  BS.pack
    [ fromIntegral (shiftR n 56) :: Word8
    , fromIntegral (shiftR n 48) :: Word8
    , fromIntegral (shiftR n 40) :: Word8
    , fromIntegral (shiftR n 32) :: Word8
    , fromIntegral (shiftR n 24) :: Word8
    , fromIntegral (shiftR n 16) :: Word8
    , fromIntegral (shiftR n  8) :: Word8
    , fromIntegral  n            :: Word8
    ]
