-- | Top-level request authentication for the ATProto OAuth provider.
--
-- Ported from the upstream TypeScript reference implementation
-- (@atproto\/oauth-provider\/src\/oauth-verifier.ts@).
--
-- Authenticates incoming HTTP requests by:
--
-- 1. Extracting and parsing the @Authorization@ header to determine the
--    token type (DPoP or Bearer).
-- 2. Verifying the DPoP proof (if present) via
--    'ATProto.OAuth.Provider.DPoP.Verifier.verifyDpopProof'.
-- 3. Decoding and verifying the access token via
--    'ATProto.OAuth.Provider.Token.verifyAccessToken'.
-- 4. Checking that the token's @cnf.jkt@ claim matches the DPoP proof's
--    JWK thumbprint (DPoP binding verification).
--
-- = Security properties
--
-- * DPoP-bound tokens (@cnf.jkt@ present) /must/ be presented with a
--   matching DPoP proof.
-- * Bearer tokens (@cnf.jkt@ absent) do not require a DPoP proof.
-- * DPoP replay detection is left to the caller (checking 'dpJti'
--   uniqueness against a store).
module ATProto.OAuth.Provider.Verifier
  ( -- * Authentication
    authenticateRequest
    -- * Parsing
  , parseAuthorizationHeader
  , TokenType (..)
  ) where

import qualified Data.Map.Strict    as Map
import qualified Data.Text          as T

import           ATProto.OAuth.Provider.Types (AccessTokenPayload (..),
                                               DpopProof (..), ProviderError (..))
import           ATProto.OAuth.Provider.DPoP.Nonce (NonceState)
import           ATProto.OAuth.Provider.DPoP.Verifier (verifyDpopProof)
import           ATProto.OAuth.Provider.Token (SigningKey, verifyAccessToken)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Token type extracted from the @Authorization@ header.
data TokenType = TokenTypeDPoP | TokenTypeBearer
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Authenticate an incoming HTTP request.
--
-- Returns the verified 'AccessTokenPayload' on success, or a
-- 'ProviderError' on failure.
--
-- The DPoP proof (if any) is returned alongside the payload so that
-- the caller can perform replay detection on 'dpJti'.
authenticateRequest
  :: SigningKey
  -> T.Text
  -- ^ Issuer URL.
  -> T.Text
  -- ^ HTTP method (e.g. @\"GET\"@).
  -> T.Text
  -- ^ Full request URL.
  -> Map.Map T.Text T.Text
  -- ^ Request headers (case-insensitive keys).
  -> Maybe NonceState
  -- ^ DPoP nonce state (pass 'Nothing' to disable nonce checks).
  -> IO (Either ProviderError (AccessTokenPayload, Maybe DpopProof))
authenticateRequest signingKey issuer httpMethod httpUrl headers mNonceState = do
  -- 1. Parse the Authorization header.
  case parseAuthorizationHeader headers of
    Left err -> return (Left err)
    Right (tokenType, token) -> do
      -- 2. Verify the DPoP proof (if present).
      dpopResult <- verifyDpopProof httpMethod httpUrl headers mNonceState
                      (Just token)
      case dpopResult of
        Left err -> return (Left err)
        Right mDpopProof -> do
          -- 3. Verify the access token.
          tokenResult <- verifyAccessToken signingKey issuer token
          case tokenResult of
            Left err -> return (Left err)
            Right tokenPayload -> do
              -- 4. Verify DPoP binding.
              case verifyBinding tokenType tokenPayload mDpopProof of
                Left err -> return (Left err)
                Right () -> return (Right (tokenPayload, mDpopProof))

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Parse the @Authorization@ header to extract the token type and value.
--
-- Supports @DPoP \<token\>@ and @Bearer \<token\>@ schemes.
parseAuthorizationHeader
  :: Map.Map T.Text T.Text
  -> Either ProviderError (TokenType, T.Text)
parseAuthorizationHeader headers =
  case Map.lookup "authorization" lowered of
    Nothing -> Left (InvalidToken "Missing Authorization header")
    Just val ->
      let stripped = T.strip val
      in case () of
        _ | "DPoP " `T.isPrefixOf` stripped ->
              Right (TokenTypeDPoP, T.strip (T.drop 5 stripped))
          | "dpop " `T.isPrefixOf` stripped ->
              Right (TokenTypeDPoP, T.strip (T.drop 5 stripped))
          | "Bearer " `T.isPrefixOf` stripped ->
              Right (TokenTypeBearer, T.strip (T.drop 7 stripped))
          | "bearer " `T.isPrefixOf` stripped ->
              Right (TokenTypeBearer, T.strip (T.drop 7 stripped))
          | otherwise ->
              Left (InvalidToken "Unsupported Authorization scheme")
  where
    lowered = Map.mapKeys T.toLower headers

-- ---------------------------------------------------------------------------
-- Binding verification
-- ---------------------------------------------------------------------------

-- | Verify the DPoP key binding between the token and the proof.
verifyBinding
  :: TokenType
  -> AccessTokenPayload
  -> Maybe DpopProof
  -> Either ProviderError ()
verifyBinding tokenType payload mDpopProof =
  case atpCnfJkt payload of
    Just jkt -> do
      -- Token is DPoP-bound; must use DPoP scheme and provide a proof.
      case tokenType of
        TokenTypeBearer ->
          Left (InvalidToken "DPoP-bound token must use DPoP authorization scheme")
        TokenTypeDPoP -> pure ()
      case mDpopProof of
        Nothing ->
          Left MissingDpopProof
        Just proof
          | dpJkt proof /= jkt ->
              Left InvalidTokenBinding
          | otherwise ->
              Right ()
    Nothing -> do
      -- Token is not DPoP-bound; must use Bearer scheme.
      case tokenType of
        TokenTypeDPoP ->
          Left (InvalidToken "Bearer token must use Bearer authorization scheme")
        TokenTypeBearer -> pure ()
      -- DPoP proofs are allowed but ignored for Bearer tokens (upstream compat).
      Right ()
