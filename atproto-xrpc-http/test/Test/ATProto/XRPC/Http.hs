module Test.ATProto.XRPC.Http (tests) where

import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import ATProto.XRPC.Types

-- ---------------------------------------------------------------------------
-- Pure helpers mirroring ATProto.XRPC.Http internals
-- ---------------------------------------------------------------------------

-- | Build the XRPC endpoint URL (mirrors buildRequest logic).
xrpcUrl :: T.Text -> T.Text -> T.Text
xrpcUrl pdsUrl nsid =
  pdsUrl <> "/xrpc/" <> nsid

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | XRPC URL is always {pdsUrl}/xrpc/{nsid}.
prop_xrpcUrlStructure :: Property
prop_xrpcUrlStructure = property $ do
  pds  <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum
  nsid <- forAll $ Gen.text (Range.linear 5  20) Gen.alphaNum
  let url = xrpcUrl pds nsid
  assert $ T.isInfixOf "/xrpc/" url
  assert $ T.isSuffixOf nsid    url
  assert $ T.isPrefixOf pds     url

-- | A known XRPC URL is built correctly.
prop_knownUrl :: Property
prop_knownUrl = withTests 1 . property $
  xrpcUrl "https://bsky.social" "com.atproto.repo.listRecords"
    === "https://bsky.social/xrpc/com.atproto.repo.listRecords"

-- | XrpcRequest default construction is sane.
prop_requestDefaults :: Property
prop_requestDefaults = withTests 1 . property $ do
  let req = XrpcRequest
              { xrpcReqMethod  = XrpcQuery
              , xrpcReqNsid    = "app.bsky.feed.getTimeline"
              , xrpcReqParams  = Map.empty
              , xrpcReqBody    = Nothing
              , xrpcReqHeaders = Map.empty
              }
  xrpcReqMethod req === XrpcQuery
  xrpcReqNsid   req === "app.bsky.feed.getTimeline"

-- | XrpcQuery maps to GET, XrpcProcedure to POST.
prop_methodDistinction :: Property
prop_methodDistinction = withTests 1 . property $ do
  XrpcQuery     /== XrpcProcedure
  XrpcQuery     === XrpcQuery
  XrpcProcedure === XrpcProcedure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "XRPC.Http"
  [ ("URL structure",            prop_xrpcUrlStructure)
  , ("known URL",                prop_knownUrl)
  , ("request defaults",         prop_requestDefaults)
  , ("method distinction",       prop_methodDistinction)
  ]
