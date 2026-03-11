module Test.ATProto.Identity.Handle (tests) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import ATProto.Identity.Handle

-- ---------------------------------------------------------------------------
-- parseDnsResult
--
-- In dns-4.x, lookupTXT returns [ByteString] where each ByteString is the
-- full TXT record (already concatenated).  parseDnsResult takes this list.
-- ---------------------------------------------------------------------------

-- | A single matching record returns the DID portion.
prop_parseDnsResultSingle :: Property
prop_parseDnsResultSingle = withTests 1 . property $
  parseDnsResult [BC.pack "did=did:plc:ewvi7nxzyoun6zhhandbv25b"]
    === Just "did:plc:ewvi7nxzyoun6zhhandbv25b"

-- | No matching record returns Nothing.
prop_parseDnsResultNoMatch :: Property
prop_parseDnsResultNoMatch = withTests 1 . property $
  parseDnsResult [BC.pack "v=spf1 include:example.com ~all"]
    === Nothing

-- | An empty record list returns Nothing.
prop_parseDnsResultEmpty :: Property
prop_parseDnsResultEmpty = withTests 1 . property $
  parseDnsResult [] === Nothing

-- | More than one matching record returns Nothing (ambiguous).
prop_parseDnsResultAmbiguous :: Property
prop_parseDnsResultAmbiguous = withTests 1 . property $
  parseDnsResult
    [ BC.pack "did=did:plc:aaa"
    , BC.pack "did=did:plc:bbb"
    ]
    === Nothing

-- | Records not starting with "did=" are ignored when one valid record exists.
prop_parseDnsResultIgnoresOthers :: Property
prop_parseDnsResultIgnoresOthers = withTests 1 . property $
  parseDnsResult
    [ BC.pack "v=spf1 ~all"
    , BC.pack "did=did:plc:ewvi7nxzyoun6zhhandbv25b"
    , BC.pack "google-site-verification=xyz"
    ]
    === Just "did:plc:ewvi7nxzyoun6zhhandbv25b"

-- | An arbitrary DID-looking value round-trips through parseDnsResult.
prop_parseDnsResultRoundTrip :: Property
prop_parseDnsResultRoundTrip = property $ do
  method <- forAll $ Gen.element ["plc", "web"]
  key    <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
  let did     = "did:" <> method <> ":" <> key
      encoded = BC.pack (T.unpack ("did=" <> did))
  parseDnsResult [encoded] === Just did

-- ---------------------------------------------------------------------------
-- handleToWellKnownUrl
-- ---------------------------------------------------------------------------

-- | Known handle maps to the correct well-known URL.
prop_wellKnownUrl :: Property
prop_wellKnownUrl = withTests 1 . property $
  handleToWellKnownUrl "alice.bsky.social"
    === "https://alice.bsky.social/.well-known/atproto-did"

-- | The URL always starts with https:// and ends with /.well-known/atproto-did.
prop_wellKnownUrlStructure :: Property
prop_wellKnownUrlStructure = property $ do
  h <- forAll $ Gen.text (Range.linear 3 30)
         (Gen.element (map T.head (T.chunksOf 1 "abcdefghijklmnopqrstuvwxyz0123456789")))
  let url = handleToWellKnownUrl h
  T.isPrefixOf "https://" (T.pack url) === True
  T.isSuffixOf "/.well-known/atproto-did" (T.pack url) === True

-- ---------------------------------------------------------------------------
-- dnsAtprotoPrefix
-- ---------------------------------------------------------------------------

prop_dnsPrefix :: Property
prop_dnsPrefix = withTests 1 . property $
  dnsAtprotoPrefix === "did="

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Identity.Handle"
  [ ("parseDnsResult: single match",     prop_parseDnsResultSingle)
  , ("parseDnsResult: no match",         prop_parseDnsResultNoMatch)
  , ("parseDnsResult: empty",            prop_parseDnsResultEmpty)
  , ("parseDnsResult: ambiguous",        prop_parseDnsResultAmbiguous)
  , ("parseDnsResult: ignores others",   prop_parseDnsResultIgnoresOthers)
  , ("parseDnsResult: round-trip",       prop_parseDnsResultRoundTrip)
  , ("well-known URL: known handle",     prop_wellKnownUrl)
  , ("well-known URL: structure",        prop_wellKnownUrlStructure)
  , ("dnsAtprotoPrefix constant",        prop_dnsPrefix)
  ]
