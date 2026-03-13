module Test.ATProto.OAuth.Types (tests) where

import qualified Data.Aeson    as Aeson
import qualified Data.Text     as T
import           Hedgehog

import ATProto.OAuth.Types

-- ---------------------------------------------------------------------------
-- JSON round-trip tests
-- ---------------------------------------------------------------------------

-- | A minimal authorization server metadata document round-trips through JSON.
prop_asmRoundTrip :: Property
prop_asmRoundTrip = withTests 1 . property $ do
  let asm = OAuthAuthorizationServerMetadata
        { asmIssuer                             = "https://bsky.social"
        , asmAuthorizationEndpoint              = "https://bsky.social/oauth/authorize"
        , asmTokenEndpoint                      = "https://bsky.social/oauth/token"
        , asmPushedAuthorizationRequestEndpoint = Just "https://bsky.social/oauth/par"
        , asmRequirePushedAuthorizationRequests = True
        , asmDpopSigningAlgValuesSupported      = ["ES256"]
        , asmTokenEndpointAuthMethodsSupported  = ["none", "private_key_jwt"]
        , asmScopesSupported                    = ["transition:generic", "transition:chat.bsky"]
        , asmResponseTypesSupported             = ["code"]
        , asmGrantTypesSupported                = ["authorization_code", "refresh_token"]
        , asmCodeChallengeMethodsSupported      = ["S256"]
        , asmClientIdMetadataDocumentSupported  = True
        }
      encoded = Aeson.toJSON asm
  case Aeson.fromJSON encoded of
    Aeson.Error err -> fail ("Round-trip failed: " ++ err)
    Aeson.Success asm' ->
      asm' === asm

-- | Authorization server metadata with missing optional fields uses defaults.
prop_asmDefaultFields :: Property
prop_asmDefaultFields = withTests 1 . property $ do
  let json = Aeson.object
        [ "issuer"                  Aeson..= ("https://bsky.social" :: T.Text)
        , "authorization_endpoint"  Aeson..= ("https://bsky.social/oauth/authorize" :: T.Text)
        , "token_endpoint"          Aeson..= ("https://bsky.social/oauth/token" :: T.Text)
        ]
  case (Aeson.fromJSON json :: Aeson.Result OAuthAuthorizationServerMetadata) of
    Aeson.Error err -> fail ("Parse failed: " ++ err)
    Aeson.Success asm -> do
      asmRequirePushedAuthorizationRequests asm === False
      asmDpopSigningAlgValuesSupported asm      === []
      asmTokenEndpointAuthMethodsSupported asm  === ["client_secret_basic"]
      asmGrantTypesSupported asm                === ["authorization_code"]
      asmClientIdMetadataDocumentSupported asm  === False

-- | Protected resource metadata round-trips through JSON.
prop_prmRoundTrip :: Property
prop_prmRoundTrip = withTests 1 . property $ do
  let prm = OAuthProtectedResourceMetadata
        { prmResource             = "https://morel.us-east.host.bsky.network"
        , prmAuthorizationServers = ["https://bsky.social"]
        }
      encoded = Aeson.toJSON prm
  case Aeson.fromJSON encoded of
    Aeson.Error err -> fail ("Round-trip failed: " ++ err)
    Aeson.Success prm' -> prm' === prm

-- | Client metadata round-trips through JSON.
prop_cmRoundTrip :: Property
prop_cmRoundTrip = withTests 1 . property $ do
  let cm = defaultClientMetadata "https://myapp.example.com"
             ["https://myapp.example.com/callback"]
      encoded = Aeson.toJSON cm
  case Aeson.fromJSON encoded of
    Aeson.Error err -> fail ("Round-trip failed: " ++ err)
    Aeson.Success cm' -> cm' === cm

-- | 'defaultClientMetadata' sets the expected defaults.
prop_defaultClientMetadataDefaults :: Property
prop_defaultClientMetadataDefaults = withTests 1 . property $ do
  let cm = defaultClientMetadata "https://myapp.example.com"
             ["https://myapp.example.com/callback"]
  cmScope cm                   === "atproto"
  cmTokenEndpointAuthMethod cm === "none"
  cmDpopBoundAccessTokens cm   === True
  cmGrantTypes cm              === ["authorization_code", "refresh_token"]
  cmResponseTypes cm           === ["code"]

tests :: Group
tests = Group "OAuth.Types"
  [ ("ASM round-trips through JSON",       prop_asmRoundTrip)
  , ("ASM defaults for missing fields",    prop_asmDefaultFields)
  , ("PRM round-trips through JSON",       prop_prmRoundTrip)
  , ("client metadata round-trips",        prop_cmRoundTrip)
  , ("defaultClientMetadata defaults",     prop_defaultClientMetadataDefaults)
  ]
