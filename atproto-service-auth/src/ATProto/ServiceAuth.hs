-- | AT Protocol inter-service authentication JWTs.
--
-- This module provides the public API for creating and verifying the
-- compact JWS tokens used for service-to-service authorization in the
-- AT Protocol network.
--
-- = Typical usage
--
-- == Creating a service JWT
--
-- @
-- import ATProto.Crypto (generateKeyPair, P256)
-- import ATProto.ServiceAuth
--
-- main :: IO ()
-- main = do
--   (priv, _pub) <- generateKeyPair P256
--   let params = ServiceJwtParams
--         { sjpIss     = "did:plc:issuer123"
--         , sjpAud     = "did:plc:audience456"
--         , sjpLxm     = Just "com.atproto.sync.getRepo"
--         , sjpKeypair = priv
--         , sjpExp     = Nothing   -- default 60 s from now
--         }
--   jwt <- createServiceJwt params
--   print jwt
-- @
--
-- == Verifying a service JWT
--
-- @
--   result <- verifyServiceJwt jwt
--               (Just "did:plc:audience456")
--               (Just "com.atproto.sync.getRepo")
--               (\\iss _refresh -> lookupKey iss)
--   case result of
--     Left  err     -> print err
--     Right payload -> print (payloadIss payload)
-- @
module ATProto.ServiceAuth
  ( -- * Creating service JWTs
    ServiceJwtParams (..)
  , createServiceJwt
  , createServiceAuthHeaders
    -- * Verifying service JWTs
  , ServiceJwtPayload (..)
  , ServiceAuthError (..)
  , verifyServiceJwt
  ) where

import ATProto.ServiceAuth.Create
import ATProto.ServiceAuth.Verify
