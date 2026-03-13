module Test.ATProto.Repo.GetProfile (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import ATProto.Lex.Json      (decode)
import ATProto.Repo.GetProfile

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A realistic getProfile response with all optional fields present.
sampleResponse :: BLC.ByteString
sampleResponse = BLC.pack $ unwords
  [ "{"
  , "  \"did\": \"did:plc:ewvi7nxzyoun6zhhandbv25b\","
  , "  \"handle\": \"haileyok.com\","
  , "  \"displayName\": \"Hailey\","
  , "  \"description\": \"Hello world\","
  , "  \"avatar\": \"https://cdn.example.com/avatar.jpg\","
  , "  \"followersCount\": 100,"
  , "  \"followsCount\": 50,"
  , "  \"postsCount\": 200"
  , "}"
  ]

-- | A minimal response with only required fields.
minimalResponse :: BLC.ByteString
minimalResponse = BLC.pack $ unwords
  [ "{"
  , "  \"did\": \"did:plc:abc\","
  , "  \"handle\": \"alice.bsky.social\""
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | The sample response parses to the expected fields.
prop_parseSampleResponse :: Property
prop_parseSampleResponse = withTests 1 . property $ do
  prof <- evalEither (decode profileViewCodec sampleResponse)
  pvDid            prof === "did:plc:ewvi7nxzyoun6zhhandbv25b"
  pvHandle         prof === "haileyok.com"
  pvDisplayName    prof === Just "Hailey"
  pvDescription    prof === Just "Hello world"
  pvAvatar         prof === Just "https://cdn.example.com/avatar.jpg"
  pvFollowersCount prof === Just 100
  pvFollowsCount   prof === Just 50
  pvPostsCount     prof === Just 200

-- | A minimal response parses with optional fields as Nothing.
prop_parseMinimalResponse :: Property
prop_parseMinimalResponse = withTests 1 . property $ do
  prof <- evalEither (decode profileViewCodec minimalResponse)
  pvDid            prof === "did:plc:abc"
  pvHandle         prof === "alice.bsky.social"
  pvDisplayName    prof === Nothing
  pvDescription    prof === Nothing
  pvAvatar         prof === Nothing
  pvFollowersCount prof === Nothing
  pvFollowsCount   prof === Nothing
  pvPostsCount     prof === Nothing

-- | Missing "did" field causes a parse error.
prop_missingDidFails :: Property
prop_missingDidFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"handle\": \"alice.bsky.social\"}"
  case decode profileViewCodec bad of
    Left  _ -> success
    Right _ -> failure

-- | Missing "handle" field causes a parse error.
prop_missingHandleFails :: Property
prop_missingHandleFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"did\": \"did:plc:abc\"}"
  case decode profileViewCodec bad of
    Left  _ -> success
    Right _ -> failure

-- | 'GetProfileParams' stores the actor.
prop_params :: Property
prop_params = withTests 1 . property $ do
  let p = GetProfileParams "did:plc:abc"
  gppActor p === "did:plc:abc"

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Repo.GetProfile"
  [ ("parse sample response",      prop_parseSampleResponse)
  , ("parse minimal response",     prop_parseMinimalResponse)
  , ("missing 'did' fails",        prop_missingDidFails)
  , ("missing 'handle' fails",     prop_missingHandleFails)
  , ("GetProfileParams",           prop_params)
  ]
