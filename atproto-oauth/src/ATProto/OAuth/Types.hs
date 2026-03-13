-- | Core data types for the ATProto OAuth 2.1 client.
--
-- This module defines the request\/response data types covering:
--
-- * 'OAuthAuthorizationServerMetadata' – RFC 8414 + PAR (RFC 9126) +
--   DPoP (RFC 9449) extensions.
-- * 'OAuthProtectedResourceMetadata' – RFC 9728.
-- * 'OAuthClientMetadata' – ATProto client ID metadata document (draft).
-- * 'TokenSet' – in-memory representation of a token response.
-- * 'OAuthError' – error type for all OAuth operations.
module ATProto.OAuth.Types
  ( -- * Server metadata
    OAuthAuthorizationServerMetadata (..)
  , OAuthProtectedResourceMetadata (..)
    -- * Client metadata
  , OAuthClientMetadata (..)
  , defaultClientMetadata
    -- * Token and session
  , TokenSet (..)
    -- * Error
  , OAuthError (..)
  ) where

import qualified Data.Aeson  as Aeson
import qualified Data.Text   as T
import           Data.Time   (UTCTime)
import           Network.HTTP.Types.URI (urlEncode)

-- ---------------------------------------------------------------------------
-- Authorization server metadata (RFC 8414 + extensions)
-- ---------------------------------------------------------------------------

-- | Authorization server metadata.
--
-- Covers the core fields from RFC 8414 §2 augmented with the extensions
-- required by the ATProto OAuth profile:
--
-- * @dpop_signing_alg_values_supported@ – RFC 9449 §5.1
-- * @pushed_authorization_request_endpoint@ – RFC 9126
-- * @require_pushed_authorization_requests@ – RFC 9126
-- * @client_id_metadata_document_supported@ – ATProto draft extension
data OAuthAuthorizationServerMetadata = OAuthAuthorizationServerMetadata
  { asmIssuer                              :: T.Text
    -- ^ @issuer@ – canonical URL of the authorization server.
  , asmAuthorizationEndpoint               :: T.Text
    -- ^ @authorization_endpoint@ – URL of the authorization endpoint.
  , asmTokenEndpoint                       :: T.Text
    -- ^ @token_endpoint@ – URL of the token endpoint.
  , asmPushedAuthorizationRequestEndpoint  :: Maybe T.Text
    -- ^ @pushed_authorization_request_endpoint@ (RFC 9126).
  , asmRequirePushedAuthorizationRequests  :: Bool
    -- ^ @require_pushed_authorization_requests@ (RFC 9126).
    -- Defaults to 'False' when absent.
  , asmDpopSigningAlgValuesSupported       :: [T.Text]
    -- ^ @dpop_signing_alg_values_supported@ (RFC 9449).
    -- Empty list means DPoP is not supported.
  , asmTokenEndpointAuthMethodsSupported   :: [T.Text]
    -- ^ @token_endpoint_auth_methods_supported@.
    -- Defaults to @[\"client_secret_basic\"]@ when absent per RFC 8414.
  , asmScopesSupported                     :: [T.Text]
    -- ^ @scopes_supported@.
  , asmResponseTypesSupported              :: [T.Text]
    -- ^ @response_types_supported@.
  , asmGrantTypesSupported                 :: [T.Text]
    -- ^ @grant_types_supported@.
    -- Defaults to @[\"authorization_code\"]@ when absent per RFC 8414.
  , asmCodeChallengeMethodsSupported       :: [T.Text]
    -- ^ @code_challenge_methods_supported@.
  , asmClientIdMetadataDocumentSupported   :: Bool
    -- ^ @client_id_metadata_document_supported@ (ATProto extension).
  } deriving (Eq, Show)

instance Aeson.FromJSON OAuthAuthorizationServerMetadata where
  parseJSON = Aeson.withObject "OAuthAuthorizationServerMetadata" $ \o ->
    OAuthAuthorizationServerMetadata
      <$> o Aeson..:  "issuer"
      <*> o Aeson..:  "authorization_endpoint"
      <*> o Aeson..:  "token_endpoint"
      <*> o Aeson..:? "pushed_authorization_request_endpoint"
      <*> (o Aeson..:? "require_pushed_authorization_requests" Aeson..!= False)
      <*> (o Aeson..:? "dpop_signing_alg_values_supported"    Aeson..!= [])
      <*> (o Aeson..:? "token_endpoint_auth_methods_supported"
                                                Aeson..!= ["client_secret_basic"])
      <*> (o Aeson..:? "scopes_supported"       Aeson..!= [])
      <*> (o Aeson..:? "response_types_supported" Aeson..!= [])
      <*> (o Aeson..:? "grant_types_supported"  Aeson..!= ["authorization_code"])
      <*> (o Aeson..:? "code_challenge_methods_supported" Aeson..!= [])
      <*> (o Aeson..:? "client_id_metadata_document_supported" Aeson..!= False)

instance Aeson.ToJSON OAuthAuthorizationServerMetadata where
  toJSON m = Aeson.object
    [ "issuer"                             Aeson..= asmIssuer m
    , "authorization_endpoint"             Aeson..= asmAuthorizationEndpoint m
    , "token_endpoint"                     Aeson..= asmTokenEndpoint m
    , "pushed_authorization_request_endpoint"
                                           Aeson..= asmPushedAuthorizationRequestEndpoint m
    , "require_pushed_authorization_requests"
                                           Aeson..= asmRequirePushedAuthorizationRequests m
    , "dpop_signing_alg_values_supported"  Aeson..= asmDpopSigningAlgValuesSupported m
    , "token_endpoint_auth_methods_supported"
                                           Aeson..= asmTokenEndpointAuthMethodsSupported m
    , "scopes_supported"                   Aeson..= asmScopesSupported m
    , "response_types_supported"           Aeson..= asmResponseTypesSupported m
    , "grant_types_supported"              Aeson..= asmGrantTypesSupported m
    , "code_challenge_methods_supported"   Aeson..= asmCodeChallengeMethodsSupported m
    , "client_id_metadata_document_supported"
                                           Aeson..= asmClientIdMetadataDocumentSupported m
    ]

-- ---------------------------------------------------------------------------
-- Protected resource metadata (RFC 9728)
-- ---------------------------------------------------------------------------

-- | Protected resource metadata returned by the PDS at
-- @\/.well-known\/oauth-protected-resource@.
--
-- The ATProto profile uses @authorization_servers[0]@ as the issuer URL for
-- the user's authorization server.
data OAuthProtectedResourceMetadata = OAuthProtectedResourceMetadata
  { prmResource             :: T.Text
    -- ^ @resource@ – the protected resource URL (the PDS endpoint).
  , prmAuthorizationServers :: [T.Text]
    -- ^ @authorization_servers@ – list of issuer URLs; ATProto always has
    -- exactly one entry.
  } deriving (Eq, Show)

instance Aeson.FromJSON OAuthProtectedResourceMetadata where
  parseJSON = Aeson.withObject "OAuthProtectedResourceMetadata" $ \o ->
    OAuthProtectedResourceMetadata
      <$> o Aeson..:  "resource"
      <*> (o Aeson..:? "authorization_servers" Aeson..!= [])

instance Aeson.ToJSON OAuthProtectedResourceMetadata where
  toJSON m = Aeson.object
    [ "resource"              Aeson..= prmResource m
    , "authorization_servers" Aeson..= prmAuthorizationServers m
    ]

-- ---------------------------------------------------------------------------
-- Client metadata (ATProto client ID metadata document draft)
-- ---------------------------------------------------------------------------

-- | OAuth client metadata served at the client_id URL.
--
-- Based on the draft @draft-ietf-oauth-client-id-metadata-document@, with
-- the fields required by the ATProto OAuth profile.
data OAuthClientMetadata = OAuthClientMetadata
  { cmClientId                  :: T.Text
    -- ^ @client_id@ – URL of this metadata document; serves as the client
    -- identifier.
  , cmRedirectUris              :: [T.Text]
    -- ^ @redirect_uris@ – allowed redirect URIs (must include the one used
    -- in authorization requests).
  , cmScope                     :: T.Text
    -- ^ @scope@ – space-separated list of scopes the client requests.
    -- Use @\"transition:generic\"@ for broad access.
  , cmGrantTypes                :: [T.Text]
    -- ^ @grant_types@ – supported grant types.
    -- Must include @\"authorization_code\"@; include
    -- @\"refresh_token\"@ if refresh tokens are desired.
  , cmResponseTypes             :: [T.Text]
    -- ^ @response_types@ – must include @\"code\"@.
  , cmTokenEndpointAuthMethod   :: T.Text
    -- ^ @token_endpoint_auth_method@ – either @\"none\"@ (public client) or
    -- @\"private_key_jwt\"@ (confidential client).
  , cmDpopBoundAccessTokens     :: Bool
    -- ^ @dpop_bound_access_tokens@ – must be @True@ for ATProto.
  , cmApplicationType           :: Maybe T.Text
    -- ^ @application_type@ – e.g. @\"web\"@ or @\"native\"@.
  , cmClientName                :: Maybe T.Text
    -- ^ @client_name@ – human-readable name of the application.
  , cmClientUri                 :: Maybe T.Text
    -- ^ @client_uri@ – URL of the application's home page.
  , cmJwksUri                   :: Maybe T.Text
    -- ^ @jwks_uri@ – URL of the client's JSON Web Key Set.
    -- Required when @token_endpoint_auth_method@ is @\"private_key_jwt\"@.
  } deriving (Eq, Show)

-- | A public client metadata template with sensible defaults.
--
-- Sets @token_endpoint_auth_method = \"none\"@,
-- @dpop_bound_access_tokens = True@, and scope @\"transition:generic\"@.
defaultClientMetadata :: T.Text -> [T.Text] -> OAuthClientMetadata
defaultClientMetadata clientId redirectUris = OAuthClientMetadata
  { cmClientId                = clientId
  , cmRedirectUris            = redirectUris
  , cmScope                   = "atproto"
  , cmGrantTypes              = ["authorization_code", "refresh_token"]
  , cmResponseTypes           = ["code"]
  , cmTokenEndpointAuthMethod = "none"
  , cmDpopBoundAccessTokens   = True
  , cmApplicationType         = Just "web"
  , cmClientName              = Nothing
  , cmClientUri               = Nothing
  , cmJwksUri                 = Nothing
  }

instance Aeson.FromJSON OAuthClientMetadata where
  parseJSON = Aeson.withObject "OAuthClientMetadata" $ \o ->
    OAuthClientMetadata
      <$> o Aeson..:  "client_id"
      <*> o Aeson..:  "redirect_uris"
      <*> (o Aeson..:? "scope" Aeson..!= "transition:generic")
      <*> (o Aeson..:? "grant_types"
                          Aeson..!= ["authorization_code", "refresh_token"])
      <*> (o Aeson..:? "response_types" Aeson..!= ["code"])
      <*> (o Aeson..:? "token_endpoint_auth_method" Aeson..!= "none")
      <*> (o Aeson..:? "dpop_bound_access_tokens"   Aeson..!= True)
      <*> o Aeson..:? "application_type"
      <*> o Aeson..:? "client_name"
      <*> o Aeson..:? "client_uri"
      <*> o Aeson..:? "jwks_uri"

instance Aeson.ToJSON OAuthClientMetadata where
  toJSON m = Aeson.object $
    [ "client_id"                   Aeson..= cmClientId m
    , "redirect_uris"               Aeson..= cmRedirectUris m
    , "scope"                       Aeson..= cmScope m
    , "grant_types"                 Aeson..= cmGrantTypes m
    , "response_types"              Aeson..= cmResponseTypes m
    , "token_endpoint_auth_method"  Aeson..= cmTokenEndpointAuthMethod m
    , "dpop_bound_access_tokens"    Aeson..= cmDpopBoundAccessTokens m
    ] ++
    maybe [] (\v -> ["application_type" Aeson..= v]) (cmApplicationType m) ++
    maybe [] (\v -> ["client_name"       Aeson..= v]) (cmClientName m) ++
    maybe [] (\v -> ["client_uri"        Aeson..= v]) (cmClientUri m) ++
    maybe [] (\v -> ["jwks_uri"          Aeson..= v]) (cmJwksUri m)

-- ---------------------------------------------------------------------------
-- Token set
-- ---------------------------------------------------------------------------

-- | An in-memory token set produced by a successful token exchange or refresh.
--
-- ATProto access tokens are always DPoP-bound (@token_type = \"DPoP\"@).
data TokenSet = TokenSet
  { tsIss          :: T.Text
    -- ^ @iss@ – issuer URL of the authorization server that issued the tokens.
  , tsSub          :: T.Text
    -- ^ @sub@ – DID of the authenticated user.
  , tsAud          :: T.Text
    -- ^ @aud@ – URL of the user's Personal Data Server (resource server).
  , tsScope        :: T.Text
    -- ^ @scope@ – the granted scope.
  , tsAccessToken  :: T.Text
    -- ^ The DPoP-bound access token.
  , tsTokenType    :: T.Text
    -- ^ Token type; always @\"DPoP\"@ for ATProto.
  , tsRefreshToken :: Maybe T.Text
    -- ^ Refresh token, when issued.
  , tsExpiresAt    :: Maybe UTCTime
    -- ^ Expiry time, derived from the @expires_in@ field.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

-- | Errors that can occur during OAuth operations.
data OAuthError
  = OAuthNetworkError String
    -- ^ A network or HTTP error occurred.
  | OAuthParseError String
    -- ^ A server response could not be parsed.
  | OAuthServerError T.Text (Maybe T.Text) Int
    -- ^ The server returned an OAuth error.
    -- Fields: error token, optional message, HTTP status code.
  | OAuthIssuerMismatch T.Text T.Text
    -- ^ The issuer in the token does not match the expected issuer.
    -- Fields: token issuer, resolved issuer.  Critical security check.
  | OAuthNoIssuer T.Text
    -- ^ Could not determine an authorization server for the PDS.
  | OAuthDpopError String
    -- ^ DPoP proof construction failed.
  | OAuthStateNotFound
    -- ^ The @state@ parameter from a callback does not match any pending flow.
  | OAuthStateMismatch
    -- ^ The @iss@ parameter from a callback does not match the expected issuer.
  | OAuthNoRefreshToken
    -- ^ The session has no refresh token; cannot refresh.
  | OAuthIdentityError String
    -- ^ Handle or DID resolution failed.
  | OAuthNoPdsEndpoint T.Text
    -- ^ The resolved DID document has no PDS service entry.
  deriving (Eq, Show)
