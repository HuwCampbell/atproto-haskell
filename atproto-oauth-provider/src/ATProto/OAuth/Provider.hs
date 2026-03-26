-- | AT Protocol OAuth 2.1 provider.
--
-- Server-side OAuth implementation for a Personal Data Server (PDS),
-- implementing DPoP proof verification (RFC 9449), DPoP-bound access
-- token creation and verification (RFC 9068), and HMAC-based rotating
-- nonces.
--
-- = Modules
--
-- * "ATProto.OAuth.Provider.Types" – Error types, configuration, and
--   token payload.
-- * "ATProto.OAuth.Provider.DPoP.Nonce" – HMAC-based rotating DPoP nonces.
-- * "ATProto.OAuth.Provider.DPoP.Verifier" – Server-side DPoP proof
--   verification.
-- * "ATProto.OAuth.Provider.Token" – Access token creation and verification.
-- * "ATProto.OAuth.Provider.Verifier" – Top-level request authentication.
--
-- = Quick start
--
-- @
-- import ATProto.OAuth.Provider
--
-- main :: IO ()
-- main = do
--   -- Generate a signing key (persist this in production).
--   signingKey <- generateSigningKey
--
--   -- Create a DPoP nonce state (share across requests).
--   nonceState <- newNonceState Nothing Nothing
--
--   -- Issue an access token bound to a DPoP key.
--   let params = CreateTokenParams
--         { ctpSub      = \"did:plc:example\"
--         , ctpAud      = \"https://pds.example.com\"
--         , ctpScope    = \"transition:generic\"
--         , ctpClientId = Just \"https://app.example.com\"
--         , ctpCnfJkt   = Just dpopKeyThumbprint
--         , ctpLifetime = 3600
--         }
--   token <- createAccessToken signingKey \"https://auth.example.com\" params
--
--   -- Authenticate an incoming request.
--   result <- authenticateRequest signingKey \"https://auth.example.com\"
--               \"GET\" \"https://pds.example.com/xrpc/app.bsky.feed.getTimeline\"
--               requestHeaders (Just nonceState)
--   case result of
--     Left err              -> handleError err
--     Right (payload, proof) -> handleSuccess payload proof
-- @
module ATProto.OAuth.Provider
  ( -- * Types
    module ATProto.OAuth.Provider.Types
    -- * DPoP nonces
  , module ATProto.OAuth.Provider.DPoP.Nonce
    -- * DPoP verification
  , module ATProto.OAuth.Provider.DPoP.Verifier
    -- * Token management
  , module ATProto.OAuth.Provider.Token
    -- * Request authentication
  , module ATProto.OAuth.Provider.Verifier
  ) where

import ATProto.OAuth.Provider.Types
import ATProto.OAuth.Provider.DPoP.Nonce
import ATProto.OAuth.Provider.DPoP.Verifier
import ATProto.OAuth.Provider.Token
import ATProto.OAuth.Provider.Verifier
