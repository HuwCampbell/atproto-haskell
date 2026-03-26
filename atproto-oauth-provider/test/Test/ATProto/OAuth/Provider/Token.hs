module Test.ATProto.OAuth.Provider.Token (tests) where

import qualified Data.Text             as T
import           Hedgehog

import ATProto.OAuth.Provider.Token
import ATProto.OAuth.Provider.Types

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | A signed access token can be verified by the same signing key.
prop_tokenRoundTrip :: Property
prop_tokenRoundTrip = withTests 5 . property $ do
  signingKey <- evalIO generateSigningKey
  let issuer = "https://auth.example.com"
      params = CreateTokenParams
        { ctpSub      = "did:plc:example123"
        , ctpAud      = "https://pds.example.com"
        , ctpScope    = "transition:generic"
        , ctpClientId = Just "https://app.example.com"
        , ctpCnfJkt   = Just "test-jkt-thumbprint"
        , ctpLifetime = 3600
        }
  eToken <- evalIO $ createAccessToken signingKey issuer params
  case eToken of
    Left err -> fail ("Token creation failed: " ++ err)
    Right token -> do
      ePayload <- evalIO $ verifyAccessToken signingKey issuer token
      case ePayload of
        Left err -> fail ("Token verification failed: " ++ show err)
        Right payload -> do
          atpIss payload      === issuer
          atpSub payload      === "did:plc:example123"
          atpAud payload      === "https://pds.example.com"
          atpScope payload    === "transition:generic"
          atpClientId payload === Just "https://app.example.com"
          atpCnfJkt payload   === Just "test-jkt-thumbprint"
          assert (not (T.null (atpJti payload)))

-- | A token without optional fields round-trips.
prop_tokenMinimalRoundTrip :: Property
prop_tokenMinimalRoundTrip = withTests 5 . property $ do
  signingKey <- evalIO generateSigningKey
  let issuer = "https://auth.example.com"
      params = CreateTokenParams
        { ctpSub      = "did:plc:minimal"
        , ctpAud      = "https://pds.example.com"
        , ctpScope    = "atproto"
        , ctpClientId = Nothing
        , ctpCnfJkt   = Nothing
        , ctpLifetime = 60
        }
  eToken <- evalIO $ createAccessToken signingKey issuer params
  case eToken of
    Left err -> fail ("Token creation failed: " ++ err)
    Right token -> do
      ePayload <- evalIO $ verifyAccessToken signingKey issuer token
      case ePayload of
        Left err -> fail ("Token verification failed: " ++ show err)
        Right payload -> do
          atpIss payload      === issuer
          atpSub payload      === "did:plc:minimal"
          atpClientId payload === Nothing
          atpCnfJkt payload   === Nothing

-- | Verification with the wrong key fails.
prop_wrongKeyFails :: Property
prop_wrongKeyFails = withTests 3 . property $ do
  signingKey1 <- evalIO generateSigningKey
  signingKey2 <- evalIO generateSigningKey
  let issuer = "https://auth.example.com"
      params = CreateTokenParams
        { ctpSub      = "did:plc:test"
        , ctpAud      = "https://pds.example.com"
        , ctpScope    = "atproto"
        , ctpClientId = Nothing
        , ctpCnfJkt   = Nothing
        , ctpLifetime = 3600
        }
  eToken <- evalIO $ createAccessToken signingKey1 issuer params
  case eToken of
    Left err -> fail ("Token creation failed: " ++ err)
    Right token -> do
      -- Verify with a different key.
      ePayload <- evalIO $ verifyAccessToken signingKey2 issuer token
      case ePayload of
        Left (InvalidToken _) -> success
        _                     -> fail "Expected InvalidToken for wrong key"

-- | Verification with the wrong issuer fails.
prop_wrongIssuerFails :: Property
prop_wrongIssuerFails = withTests 3 . property $ do
  signingKey <- evalIO generateSigningKey
  let params = CreateTokenParams
        { ctpSub      = "did:plc:test"
        , ctpAud      = "https://pds.example.com"
        , ctpScope    = "atproto"
        , ctpClientId = Nothing
        , ctpCnfJkt   = Nothing
        , ctpLifetime = 3600
        }
  eToken <- evalIO $ createAccessToken signingKey "https://auth1.example.com" params
  case eToken of
    Left err -> fail ("Token creation failed: " ++ err)
    Right token -> do
      -- Verify with a different issuer.
      ePayload <- evalIO $ verifyAccessToken signingKey "https://auth2.example.com" token
      case ePayload of
        Left (InvalidToken msg) -> assert (T.isInfixOf "Issuer" msg || T.isInfixOf "issuer" msg || T.isInfixOf "iss" msg)
        _                       -> fail "Expected InvalidToken for wrong issuer"

-- | A garbled token string is rejected.
prop_garbageTokenFails :: Property
prop_garbageTokenFails = withTests 1 . property $ do
  signingKey <- evalIO generateSigningKey
  ePayload <- evalIO $ verifyAccessToken signingKey "https://example.com" "not.a.token"
  case ePayload of
    Left (InvalidToken _) -> success
    _                     -> fail "Expected InvalidToken for garbage input"

-- | Two tokens have different jti values.
prop_jtiUnique :: Property
prop_jtiUnique = withTests 5 . property $ do
  signingKey <- evalIO generateSigningKey
  let issuer = "https://auth.example.com"
      params = CreateTokenParams
        { ctpSub      = "did:plc:test"
        , ctpAud      = "https://pds.example.com"
        , ctpScope    = "atproto"
        , ctpClientId = Nothing
        , ctpCnfJkt   = Nothing
        , ctpLifetime = 3600
        }
  eToken1 <- evalIO $ createAccessToken signingKey issuer params
  eToken2 <- evalIO $ createAccessToken signingKey issuer params
  case (eToken1, eToken2) of
    (Right t1, Right t2) -> t1 /== t2
    _                    -> fail "Token creation failed"

tests :: Group
tests = Group "OAuth.Provider.Token"
  [ ("token round-trips",                prop_tokenRoundTrip)
  , ("minimal token round-trips",        prop_tokenMinimalRoundTrip)
  , ("wrong key fails verification",     prop_wrongKeyFails)
  , ("wrong issuer fails verification",  prop_wrongIssuerFails)
  , ("garbage token fails",              prop_garbageTokenFails)
  , ("jti values are unique",            prop_jtiUnique)
  ]
