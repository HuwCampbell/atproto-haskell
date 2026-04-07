-- | Service-auth JWT verification for the feed generator.
--
-- Provides 'makeServiceAuthVerifier', which validates AT Protocol
-- inter-service JWTs carried in the @Authorization: Bearer \<token\>@
-- request header.  On success the issuer DID is returned as the caller
-- identity; on failure an appropriate XRPC auth error is returned.
module FeedGenerator.Auth
  ( makeServiceAuthVerifier
  ) where

import           Control.Monad.IO.Class    (liftIO)
import qualified Data.CaseInsensitive      as CI
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T

import           ATProto.ServiceAuth       (ServiceJwtPayload (..), verifyServiceJwt)
import           ATProto.XRPC.Types        (XrpcHeaders)
import           ATProto.XRPC.Server.Types (AuthResult (..), AuthVerifier)

import           FeedGenerator.Types       (AppEnv (..), AppM)

-- | Build an 'AuthVerifier' that validates AT Protocol service-auth JWTs.
--
-- The verifier:
--
-- 1. Extracts the @Bearer@ token from the @Authorization@ header.
-- 2. Calls 'verifyServiceJwt' with this generator's DID as the expected
--    audience and @\"app.bsky.feed.getFeedSkeleton\"@ as the expected lxm.
-- 3. Returns 'AuthOk' with the issuer DID on success, or 'AuthFailed' on
--    any error (missing header, bad token, expired, wrong audience, etc.).
makeServiceAuthVerifier :: AppEnv -> AuthVerifier AppM T.Text
makeServiceAuthVerifier env headers = liftIO $ do
  case Map.lookup (CI.mk "authorization") headers of
    Nothing ->
      return $ AuthFailed "AuthRequired" (Just "Authorization header missing")
    Just authVal ->
      case T.stripPrefix "Bearer " authVal of
        Nothing ->
          return $ AuthFailed "AuthRequired" (Just "Expected Bearer token")
        Just jwt -> do
          result <- verifyServiceJwt
                      jwt
                      (Just (envServiceDid env))
                      (Just "app.bsky.feed.getFeedSkeleton")
                      (envDidResolver env)
          return $ case result of
            Left  err -> AuthFailed "AuthRequired" (Just (T.pack (show err)))
            Right pay -> AuthOk (Just (payloadIss pay))
