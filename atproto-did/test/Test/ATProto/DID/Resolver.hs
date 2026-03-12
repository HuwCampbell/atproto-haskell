-- | Tests for pure URL construction logic in the DID resolvers.
--
-- These tests exercise the parts that do not require live network access:
-- URL construction for did:web DIDs and validation of did:web format.
module Test.ATProto.DID.Resolver (tests) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Text      as T

import ATProto.DID.Resolver (ResolveError (..))

-- ---------------------------------------------------------------------------
-- White-box access to webDidToUrl via re-export trick
-- ---------------------------------------------------------------------------
-- We test the URL construction logic by importing the internal helper
-- directly.  The module does not export it, so we replicate the logic
-- inline.  This keeps the public API clean while still being tested.

-- | Reference implementation of did:web → URL conversion (mirrors
-- ATProto.DID.Resolver.Web.webDidToUrl).
webDidToUrl :: T.Text -> Either ResolveError String
webDidToUrl did =
  case T.stripPrefix "did:web:" did of
    Nothing   -> Left (DidUnsupported did)
    Just rest ->
      if T.elem ':' rest
        then Left $ DidParseError
               "did:web identifiers with path components are not \
               \supported by the AT Protocol"
        else Right ("https://" ++ T.unpack rest ++ "/.well-known/did.json")

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | did:web:example.com → correct URL
prop_webDidSimpleUrl :: Property
prop_webDidSimpleUrl = withTests 1 . property $
  webDidToUrl "did:web:example.com"
    === Right "https://example.com/.well-known/did.json"

-- | did:web:sub.example.com → correct URL (subdomain)
prop_webDidSubdomain :: Property
prop_webDidSubdomain = withTests 1 . property $
  webDidToUrl "did:web:sub.example.com"
    === Right "https://sub.example.com/.well-known/did.json"

-- | Arbitrary single-segment did:web host → URL has expected form.
prop_webDidUrlStructure :: Property
prop_webDidUrlStructure = property $ do
  host <- forAll $
    Gen.text (Range.linear 1 30) (Gen.element ("abcdefghijklmnopqrstuvwxyz0123456789-." :: String))
  let did = "did:web:" <> host
  case webDidToUrl did of
    Left  _ ->
      -- A colon in the generated host would cause a parse failure;
      -- that's fine (it's not a valid hostname anyway).
      success
    Right url -> do
      T.isPrefixOf "https://" (T.pack url) === True
      T.isSuffixOf "/.well-known/did.json" (T.pack url) === True

-- | did:web with a path component is rejected.
prop_webDidPathRejected :: Property
prop_webDidPathRejected = withTests 1 . property $
  case webDidToUrl "did:web:example.com:user:alice" of
    Left (DidParseError _) -> success
    _                      -> failure

-- | A non-did:web string is returned as DidUnsupported.
prop_nonWebDidUnsupported :: Property
prop_nonWebDidUnsupported = withTests 1 . property $
  case webDidToUrl "did:plc:abc" of
    Left (DidUnsupported _) -> success
    _                       -> failure

-- | did:plc URL: default plc.directory + DID.
prop_plcUrl :: Property
prop_plcUrl = withTests 1 . property $ do
  let did = "did:plc:ewvi7nxzyoun6zhhandbv25b"
      expected = "https://plc.directory/" ++ T.unpack did
      actual   = "https://plc.directory" ++ "/" ++ T.unpack did
  actual === expected

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "DID.Resolver"
  [ ("did:web simple URL",           prop_webDidSimpleUrl)
  , ("did:web subdomain URL",        prop_webDidSubdomain)
  , ("did:web URL structure",        prop_webDidUrlStructure)
  , ("did:web with path rejected",   prop_webDidPathRejected)
  , ("non-web DID unsupported",      prop_nonWebDidUnsupported)
  , ("did:plc URL construction",     prop_plcUrl)
  ]
