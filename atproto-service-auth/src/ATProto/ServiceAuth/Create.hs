-- | Create AT Protocol inter-service authentication JWTs.
--
-- Service auth tokens are compact JWS (JSON Web Signature) values with
-- an ECDSA signature over the base64url-encoded header and payload.
-- The two supported algorithms are ES256 (P-256) and ES256K (secp256k1).
module ATProto.ServiceAuth.Create
  ( ServiceJwtParams (..)
  , createServiceJwt
  , createServiceAuthHeaders
  ) where

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64.URL  as Base64URL
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           Data.Word                   (Word8)
import           Crypto.Random               (getRandomBytes)

import           ATProto.Crypto.EC           (sign)
import           ATProto.Crypto.Types        (PrivKey (..), Signature (..), jwtAlg)

-- | Parameters for creating a service authentication JWT.
data ServiceJwtParams = ServiceJwtParams
  { sjpIss     :: T.Text
    -- ^ Issuer DID, possibly with a @#fragment@ for key id.
  , sjpAud     :: T.Text
    -- ^ Audience DID.
  , sjpLxm     :: Maybe T.Text
    -- ^ Optional lexicon method NSID.
  , sjpKeypair :: PrivKey
    -- ^ Private key used to sign the token.
  , sjpExp     :: Maybe Int
    -- ^ Optional explicit expiry (seconds since epoch).
    -- When 'Nothing', defaults to 60 seconds from now.
  } deriving (Show)

-- | Create a service authentication JWT.
--
-- The resulting token is a compact JWS string of the form
-- @header.payload.signature@, where each segment is base64url-encoded
-- without padding.
createServiceJwt :: ServiceJwtParams -> IO T.Text
createServiceJwt params = do
  now <- round <$> getPOSIXTime
  jtiHex <- randomHex 16
  let curve   = privKeyCurve (sjpKeypair params)
      alg     = T.pack (jwtAlg curve)
      expTime = maybe (now + 60) id (sjpExp params)

      headerJson = Aeson.object
        [ "typ" Aeson..= ("JWT" :: T.Text)
        , "alg" Aeson..= alg
        ]

      basePairs =
        [ "iss" Aeson..= sjpIss params
        , "aud" Aeson..= sjpAud params
        , "exp" Aeson..= expTime
        , "iat" Aeson..= (now :: Int)
        , "jti" Aeson..= jtiHex
        ]

      payloadPairs = case sjpLxm params of
        Nothing  -> basePairs
        Just lxm -> basePairs ++ ["lxm" Aeson..= lxm]

      payloadJson = Aeson.object payloadPairs

      headerB64  = base64urlEncode (BL.toStrict (Aeson.encode headerJson))
      payloadB64 = base64urlEncode (BL.toStrict (Aeson.encode payloadJson))

      sigInput = headerB64 <> "." <> payloadB64

  sig <- sign (sjpKeypair params) sigInput
  let sigB64 = base64urlEncode (unSignature sig)

  return $ TE.decodeUtf8 (sigInput <> "." <> sigB64)

-- | Create a service JWT and return it as a Bearer authorization header.
--
-- Returns a @(name, value)@ pair suitable for use as an HTTP header:
-- @(\"Authorization\", \"Bearer \<jwt\>\")@.
createServiceAuthHeaders :: ServiceJwtParams -> IO (T.Text, T.Text)
createServiceAuthHeaders params = do
  jwt <- createServiceJwt params
  return ("Authorization", "Bearer " <> jwt)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Base64url encode without padding.
base64urlEncode :: BS.ByteString -> BS.ByteString
base64urlEncode = BS.dropWhileEnd (== 0x3D) . Base64URL.encode

-- | Generate @n@ random bytes and return them as a hex-encoded 'T.Text'.
randomHex :: Int -> IO T.Text
randomHex n = do
  bytes <- getRandomBytes n
  return $ bytesToHex bytes

-- | Encode a 'BS.ByteString' as lower-case hexadecimal text.
bytesToHex :: BS.ByteString -> T.Text
bytesToHex = TE.decodeUtf8 . BS.concatMap wordToHex
  where
    wordToHex :: Word8 -> BS.ByteString
    wordToHex w =
      let (hi, lo) = w `divMod` 16
      in BS.pack [hexDigit hi, hexDigit lo]

    hexDigit :: Word8 -> Word8
    hexDigit d
      | d < 10    = d + 0x30       -- '0'..'9'
      | otherwise = d - 10 + 0x61  -- 'a'..'f'
