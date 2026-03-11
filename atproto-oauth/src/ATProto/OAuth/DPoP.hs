-- | DPoP (Demonstrated Proof of Possession) proof generation.
--
-- Implements RFC 9449 – the DPoP mechanism used by the ATProto OAuth profile.
-- Every token endpoint request and every resource request must include a
-- DPoP proof JWT in the @DPoP@ header.
--
-- = Security properties
--
-- * The DPoP key is an ephemeral EC P-256 keypair generated fresh per session.
-- * Each proof carries a unique @jti@ (JWT ID) to prevent replay.
-- * The @htm@ and @htu@ claims bind the proof to a specific HTTP method and
--   URL (no query string, no fragment).
-- * The @ath@ claim (base64url-encoded SHA-256 of the access token) is
--   included in resource-server proofs but not in token-endpoint proofs.
-- * The @nonce@ claim is included when the server has previously sent a
--   @DPoP-Nonce@ header.
--
-- = Usage
--
-- @
-- import ATProto.OAuth.DPoP
--
-- main :: IO ()
-- main = do
--   dpopKey <- generateDpopKey
--   proof   <- createDpopProof dpopKey DpopClaims
--     { dcHtm   = \"POST\"
--     , dcHtu   = \"https://bsky.social/oauth/token\"
--     , dcNonce = Nothing
--     , dcAth   = Nothing
--     }
--   case proof of
--     Left  err -> putStrLn (\"DPoP error: \" ++ err)
--     Right tok -> putStrLn (\"DPoP: \" ++ show tok)
-- @
module ATProto.OAuth.DPoP
  ( -- * Key management
    DpopKey
  , generateDpopKey
  , dpopPublicJwk
    -- * Proof parameters
  , DpopClaims (..)
    -- * Proof creation
  , createDpopProof
    -- * Helpers (exported for testing)
  , generateJti
  , accessTokenHash
  ) where

import           Control.Lens              (set, view)
import qualified Crypto.Hash               as Hash
import           Crypto.Hash.Algorithms    (SHA256 (..))
import qualified Crypto.Random             as Random
import           Crypto.JOSE.Compact       (encodeCompact)
import           Crypto.JOSE.Error         (Error, runJOSE)
import           Crypto.JOSE.Header        (HeaderParam (..), HasJwk (jwk),
                                            HasTyp (typ))
import           Crypto.JOSE.JWK           (Crv (..), JWK, KeyMaterialGenParam (..),
                                            AsPublicKey (..), genJWK)
import           Crypto.JOSE.JWS           (Alg (..), CompactJWS, JWSHeader,
                                            RequiredProtection (..),
                                            newJWSHeaderProtected, signJWS)
import qualified Data.Aeson                as Aeson
import qualified Data.ByteArray            as BA
import           Data.ByteArray.Encoding   (Base (..), convertToBase)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Functor.Identity     as Identity
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Time.Clock.POSIX     (getPOSIXTime)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | An ephemeral EC P-256 keypair used for DPoP proofs.
--
-- A 'DpopKey' is created once per session (or per authorization server) and
-- reused for all DPoP proofs in that session.  Use 'generateDpopKey' to
-- create a fresh key.
newtype DpopKey = DpopKey JWK

-- | Extract the public-key-only JWK from a 'DpopKey'.
--
-- Returns 'Nothing' when the key material does not support the public\/private
-- key split (should not happen for EC keys).
dpopPublicJwk :: DpopKey -> Maybe JWK
dpopPublicJwk (DpopKey k) = view asPublicKey k

-- | Parameters for a single DPoP proof.
data DpopClaims = DpopClaims
  { dcHtm   :: T.Text
    -- ^ @htm@ – the HTTP method of the request (e.g. @\"POST\"@).
  , dcHtu   :: T.Text
    -- ^ @htu@ – the HTTP URI of the request.  Must not include query
    -- parameters or a fragment component (RFC 9449 §4.2).
  , dcNonce :: Maybe T.Text
    -- ^ @nonce@ – server-supplied nonce.  Include this when the server
    -- previously sent a @DPoP-Nonce@ response header.
  , dcAth   :: Maybe T.Text
    -- ^ @ath@ – access token hash.
    -- Required for resource-server requests; omit for token-endpoint requests.
    -- Compute with 'accessTokenHash'.
  }

-- ---------------------------------------------------------------------------
-- Key generation
-- ---------------------------------------------------------------------------

-- | Generate a fresh DPoP keypair using the OS CSPRNG.
--
-- The key uses EC P-256 (ES256), which is the fallback algorithm required by
-- the ATProto OAuth specification.
generateDpopKey :: IO DpopKey
generateDpopKey = DpopKey <$> genJWK (ECGenParam P_256)

-- ---------------------------------------------------------------------------
-- Proof creation
-- ---------------------------------------------------------------------------

-- | Create a DPoP proof JWT for the given claims.
--
-- Produces a compact JWS (three base64url-encoded segments separated by
-- dots) signed with the DPoP private key.
--
-- The proof header is:
--
-- @
-- { \"typ\": \"dpop+jwt\", \"alg\": \"ES256\", \"jwk\": \<public-key\> }
-- @
--
-- The proof payload is:
--
-- @
-- { \"jti\": \<random\>, \"htm\": \<method\>, \"htu\": \<url\>,
--   \"iat\": \<unix-seconds\>, [\"nonce\": \<nonce\>], [\"ath\": \<ath\>] }
-- @
createDpopProof
  :: DpopKey
  -> DpopClaims
  -> IO (Either String T.Text)
createDpopProof (DpopKey dpopKey) claims = do
  jti <- generateJti
  iat <- fmap (floor :: Double -> Int) (fmap realToFrac getPOSIXTime)

  -- Build the DPoP header.
  let pubJwk = view asPublicKey dpopKey
      hdr0   = newJWSHeaderProtected ES256 :: JWSHeader RequiredProtection
      hdr1   = set typ (Just (HeaderParam RequiredProtection "dpop+jwt")) hdr0
      hdr2   = case pubJwk of
                 Just pk -> set jwk (Just (HeaderParam RequiredProtection pk)) hdr1
                 Nothing -> hdr1

  -- Build the DPoP payload.
  let payloadValue = buildPayload jti iat claims
      payloadBytes = BL.toStrict (Aeson.encode payloadValue)

  -- Sign.
  result <- runJOSE (signJWS payloadBytes (Identity.Identity (hdr2, dpopKey)))
              :: IO (Either Error (CompactJWS JWSHeader))
  case result of
    Left  err -> return (Left (show err))
    Right jws ->
      let compactBytes = BL.toStrict (encodeCompact jws)
      in  return (Right (TE.decodeUtf8Lenient compactBytes))

-- | Build the DPoP payload as an Aeson value.
buildPayload :: T.Text -> Int -> DpopClaims -> Aeson.Value
buildPayload jti iat claims =
  Aeson.object $
    [ "jti" Aeson..= jti
    , "htm" Aeson..= dcHtm claims
    , "htu" Aeson..= dcHtu claims
    , "iat" Aeson..= iat
    ] ++
    maybe [] (\n -> ["nonce" Aeson..= n]) (dcNonce claims) ++
    maybe [] (\a -> ["ath"   Aeson..= a]) (dcAth   claims)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Generate a random JWT ID (@jti@) as a 16-byte lowercase hex string.
--
-- The @jti@ prevents proof replay: each proof must carry a unique identifier
-- that the server can optionally track.
generateJti :: IO T.Text
generateJti = do
  bytes <- Random.getRandomBytes 16 :: IO BS.ByteString
  let hexBytes = convertToBase Base16 bytes :: BS.ByteString
  return (TE.decodeUtf8Lenient hexBytes)

-- | Compute the @ath@ claim value: base64url-unpadded SHA-256 of the access
-- token bytes.
--
-- This is required in DPoP proofs sent to resource servers so that the server
-- can verify the proof is bound to the specific access token being presented.
--
-- Returns a 'BS.ByteString' suitable for use as the @ath@ value in
-- 'DpopClaims'.
accessTokenHash :: T.Text -> BS.ByteString
accessTokenHash token =
  let digest = Hash.hashWith SHA256 (TE.encodeUtf8 token) :: Hash.Digest SHA256
  in  convertToBase Base64URLUnpadded (BA.convert digest :: BS.ByteString)
