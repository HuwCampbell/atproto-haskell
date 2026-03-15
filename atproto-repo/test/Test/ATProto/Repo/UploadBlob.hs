-- | Unit tests for 'ATProto.Repo.UploadBlob'.
module Test.ATProto.Repo.UploadBlob (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import           ATProto.Ipld.Value  (BlobRef (..), Cid (..))
import           ATProto.Lex.Json    (decode)
import           ATProto.Repo.UploadBlob

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A realistic uploadBlob response from the server.
sampleBlobResponse :: BLC.ByteString
sampleBlobResponse = BLC.pack $ concat
  [ "{"
  , "  \"blob\": {"
  , "    \"$type\": \"blob\","
  , "    \"ref\": { \"$link\": \"bafkreiabc123\" },"
  , "    \"mimeType\": \"image/jpeg\","
  , "    \"size\": 54321"
  , "  }"
  , "}"
  ]

-- | A response that is missing the \"blob\" field entirely.
missingBlobResponse :: BLC.ByteString
missingBlobResponse = BLC.pack "{\"other\": \"field\"}"

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | The sample response parses successfully.
prop_parseSampleResponse :: Property
prop_parseSampleResponse = withTests 1 . property $ do
  _ <- evalEither (decode uploadBlobResponseCodec sampleBlobResponse)
  success

-- | Missing \"blob\" field causes a parse error.
prop_missingBlobFails :: Property
prop_missingBlobFails = withTests 1 . property $ do
  case decode uploadBlobResponseCodec missingBlobResponse of
    Left  _ -> success
    Right _ -> failure

-- | The decoded BlobRef has the expected CID, MIME type, and size.
prop_blobRefFields :: Property
prop_blobRefFields = withTests 1 . property $ do
  resp <- evalEither (decode uploadBlobResponseCodec sampleBlobResponse)
  let b = ubrBlob resp
  cidText (blobRefCid b)  === "bafkreiabc123"
  blobRefMimeType b       === "image/jpeg"
  blobRefSize b           === 54321

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Repo.UploadBlob"
  [ ("parse sample response",   prop_parseSampleResponse)
  , ("missing blob field fails", prop_missingBlobFails)
  , ("blob ref fields",         prop_blobRefFields)
  ]
