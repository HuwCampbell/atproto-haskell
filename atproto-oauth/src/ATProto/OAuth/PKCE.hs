-- | PKCE (Proof Key for Code Exchange) implementation for the ATProto OAuth
-- client.
--
-- Implements RFC 7636 §4 using the S256 code challenge method, which is the
-- only method permitted by the ATProto OAuth profile.
--
-- The code verifier is a cryptographically random 32-byte value encoded as
-- base64url without padding (RFC 7636 §4.1).  The code challenge is computed
-- as:
--
-- @
-- code_challenge = BASE64URL(SHA-256(ASCII(code_verifier)))
-- @
--
-- = Usage
--
-- @
-- import ATProto.OAuth.PKCE
--
-- main :: IO ()
-- main = do
--   verifier   <- generateCodeVerifier
--   let challenge = codeChallenge verifier
--   -- Send challenge in the authorization request.
--   -- Send verifier in the token exchange request.
-- @
module ATProto.OAuth.PKCE
  ( -- * Code verifier
    generateCodeVerifier
    -- * Code challenge
  , codeChallenge
  ) where

import qualified Crypto.Hash                  as Hash
import           Crypto.Hash.Algorithms       (SHA256 (..))
import qualified Crypto.Random                as Random
import qualified Data.ByteArray               as BA
import           Data.ByteArray.Encoding      (Base (..), convertToBase)
import qualified Data.ByteString              as BS

-- | Generate a fresh PKCE code verifier.
--
-- The verifier is 32 cryptographically random bytes encoded as
-- base64url without padding, producing a 43-character string.
--
-- The returned 'BS.ByteString' is suitable for direct use as the
-- @code_verifier@ query parameter.
generateCodeVerifier :: IO BS.ByteString
generateCodeVerifier = do
  bytes <- Random.getRandomBytes 32 :: IO BS.ByteString
  return (convertToBase Base64URLUnpadded bytes)

-- | Compute the PKCE S256 code challenge from a code verifier.
--
-- @
-- codeChallenge verifier = BASE64URL_UNPADDED(SHA-256(verifier))
-- @
--
-- The input is the raw verifier bytes (as returned by 'generateCodeVerifier').
-- The output is the base64url-unpadded SHA-256 digest, suitable for use
-- as the @code_challenge@ query parameter with @code_challenge_method=S256@.
codeChallenge :: BS.ByteString -> BS.ByteString
codeChallenge verifier =
  let digest = Hash.hashWith SHA256 verifier :: Hash.Digest SHA256
  in  convertToBase Base64URLUnpadded (BA.convert digest :: BS.ByteString)
