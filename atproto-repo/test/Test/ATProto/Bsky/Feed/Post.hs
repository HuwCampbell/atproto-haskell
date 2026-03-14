module Test.ATProto.Bsky.Feed.Post (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import ATProto.Lex.Json          (decode, encode)
import ATProto.Bsky.Feed.Post

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A minimal post with only the two required fields.
minimalPost :: BLC.ByteString
minimalPost = BLC.pack $ unwords
  [ "{"
  , "  \"$type\": \"app.bsky.feed.post\","
  , "  \"text\": \"Hello, Bluesky!\","
  , "  \"createdAt\": \"2024-01-15T12:00:00.000Z\""
  , "}"
  ]

-- | A post that is a reply to another post.
replyPost :: BLC.ByteString
replyPost = BLC.pack $ unwords
  [ "{"
  , "  \"$type\": \"app.bsky.feed.post\","
  , "  \"text\": \"Replying here.\","
  , "  \"createdAt\": \"2024-01-15T12:01:00.000Z\","
  , "  \"reply\": {"
  , "    \"root\":   { \"uri\": \"at://did:plc:root/app.bsky.feed.post/root\",   \"cid\": \"bafyreiroot\" },"
  , "    \"parent\": { \"uri\": \"at://did:plc:par/app.bsky.feed.post/parent\", \"cid\": \"bafyreiparent\" }"
  , "  }"
  , "}"
  ]

-- | A post with all three facet feature types: mention, link, and tag.
facetPost :: BLC.ByteString
facetPost = BLC.pack $ unwords
  [ "{"
  , "  \"$type\": \"app.bsky.feed.post\","
  , "  \"text\": \"Hello @alice.bsky.social, see https://example.com #haskell\","
  , "  \"createdAt\": \"2024-01-15T12:02:00.000Z\","
  , "  \"facets\": ["
  , "    { \"index\": { \"byteStart\": 6, \"byteEnd\": 24 },"
  , "      \"features\": [ { \"$type\": \"app.bsky.richtext.facet#mention\", \"did\": \"did:plc:alice\" } ]"
  , "    },"
  , "    { \"index\": { \"byteStart\": 30, \"byteEnd\": 49 },"
  , "      \"features\": [ { \"$type\": \"app.bsky.richtext.facet#link\", \"uri\": \"https://example.com\" } ]"
  , "    },"
  , "    { \"index\": { \"byteStart\": 50, \"byteEnd\": 57 },"
  , "      \"features\": [ { \"$type\": \"app.bsky.richtext.facet#tag\", \"tag\": \"haskell\" } ]"
  , "    }"
  , "  ]"
  , "}"
  ]

-- | A post with an images embed (two images, one with an aspect ratio).
imagesPost :: BLC.ByteString
imagesPost = BLC.pack $ unwords
  [ "{"
  , "  \"$type\": \"app.bsky.feed.post\","
  , "  \"text\": \"Check out these photos!\","
  , "  \"createdAt\": \"2024-01-15T12:03:00.000Z\","
  , "  \"embed\": {"
  , "    \"$type\": \"app.bsky.embed.images\","
  , "    \"images\": ["
  , "      { \"image\": { \"$type\": \"blob\", \"ref\": { \"$link\": \"bafyreisunset\" }, \"mimeType\": \"image/jpeg\", \"size\": 123456 },"
  , "        \"alt\": \"A beautiful sunset\","
  , "        \"aspectRatio\": { \"width\": 1920, \"height\": 1080 }"
  , "      },"
  , "      { \"image\": { \"$type\": \"blob\", \"ref\": { \"$link\": \"bafyreisunrise\" }, \"mimeType\": \"image/jpeg\", \"size\": 98765 },"
  , "        \"alt\": \"A bright sunrise\""
  , "      }"
  , "    ]"
  , "  }"
  , "}"
  ]

-- | A post with an external link embed (all fields including thumb).
externalPost :: BLC.ByteString
externalPost = BLC.pack $ unwords
  [ "{"
  , "  \"$type\": \"app.bsky.feed.post\","
  , "  \"text\": \"Interesting article!\","
  , "  \"createdAt\": \"2024-01-15T12:04:00.000Z\","
  , "  \"embed\": {"
  , "    \"$type\": \"app.bsky.embed.external\","
  , "    \"external\": {"
  , "      \"uri\": \"https://example.com/article\","
  , "      \"title\": \"An interesting article\","
  , "      \"description\": \"This is the article description.\","
  , "      \"thumb\": { \"$type\": \"blob\", \"ref\": { \"$link\": \"bafyreithumb\" }, \"mimeType\": \"image/jpeg\", \"size\": 5000 }"
  , "    }"
  , "  }"
  , "}"
  ]

-- | A post with a record (quote-post) embed.
recordPost :: BLC.ByteString
recordPost = BLC.pack $ unwords
  [ "{"
  , "  \"$type\": \"app.bsky.feed.post\","
  , "  \"text\": \"Quoting this great post!\","
  , "  \"createdAt\": \"2024-01-15T12:05:00.000Z\","
  , "  \"embed\": {"
  , "    \"$type\": \"app.bsky.embed.record\","
  , "    \"record\": {"
  , "      \"uri\": \"at://did:plc:quoted/app.bsky.feed.post/rkey\","
  , "      \"cid\": \"bafyreiquoted\""
  , "    }"
  , "  }"
  , "}"
  ]

-- | A post declaring languages and tags.
langsTagsPost :: BLC.ByteString
langsTagsPost = BLC.pack $ unwords
  [ "{"
  , "  \"$type\": \"app.bsky.feed.post\","
  , "  \"text\": \"#haskell #atproto is great!\","
  , "  \"createdAt\": \"2024-01-15T12:06:00.000Z\","
  , "  \"langs\": [\"en\"],"
  , "  \"tags\": [\"haskell\", \"atproto\"]"
  , "}"
  ]

-- | A post with a self-labels union value.
labelsPost :: BLC.ByteString
labelsPost = BLC.pack $ unwords
  [ "{"
  , "  \"$type\": \"app.bsky.feed.post\","
  , "  \"text\": \"Labelled post.\","
  , "  \"createdAt\": \"2024-01-15T12:07:00.000Z\","
  , "  \"labels\": {"
  , "    \"$type\": \"com.atproto.label.defs#selfLabels\","
  , "    \"values\": [ { \"val\": \"!adult\" } ]"
  , "  }"
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | A minimal post with only required fields decodes correctly.
prop_parseMinimalPost :: Property
prop_parseMinimalPost = withTests 1 . property $ do
  post <- evalEither (decode feedPostCodec minimalPost)
  fpText      post === "Hello, Bluesky!"
  fpCreatedAt post === "2024-01-15T12:00:00.000Z"
  fpReply     post === Nothing
  fpFacets    post === Nothing
  fpEmbed     post === Nothing
  fpLangs     post === Nothing
  fpLabels    post === Nothing
  fpTags      post === Nothing

-- | A reply post decodes root and parent strong refs correctly.
prop_parseReplyPost :: Property
prop_parseReplyPost = withTests 1 . property $ do
  post <- evalEither (decode feedPostCodec replyPost)
  fpText post === "Replying here."
  case fpReply post of
    Nothing -> failure
    Just rr -> do
      srUri (rrRoot   rr) === "at://did:plc:root/app.bsky.feed.post/root"
      srCid (rrRoot   rr) === "bafyreiroot"
      srUri (rrParent rr) === "at://did:plc:par/app.bsky.feed.post/parent"
      srCid (rrParent rr) === "bafyreiparent"

-- | A post with facets covering mention, link, and tag features decodes correctly.
prop_parseFacetPost :: Property
prop_parseFacetPost = withTests 1 . property $ do
  post <- evalEither (decode feedPostCodec facetPost)
  case fpFacets post of
    Nothing   -> failure
    Just facs -> do
      length facs === 3
      -- First facet: mention
      let f0 = facs !! 0
      bsByteStart (facIndex f0) === 6
      bsByteEnd   (facIndex f0) === 24
      case facFeatures f0 of
        [FacetFeatureMention m] -> fmDid m === "did:plc:alice"
        _                       -> failure
      -- Second facet: link
      let f1 = facs !! 1
      bsByteStart (facIndex f1) === 30
      bsByteEnd   (facIndex f1) === 49
      case facFeatures f1 of
        [FacetFeatureLink l] -> flUri l === "https://example.com"
        _                    -> failure
      -- Third facet: tag
      let f2 = facs !! 2
      bsByteStart (facIndex f2) === 50
      bsByteEnd   (facIndex f2) === 57
      case facFeatures f2 of
        [FacetFeatureTag t] -> ftTag t === "haskell"
        _                   -> failure

-- | A post with an images embed decodes image alt text and aspect ratio.
prop_parseImagesPost :: Property
prop_parseImagesPost = withTests 1 . property $ do
  post <- evalEither (decode feedPostCodec imagesPost)
  case fpEmbed post of
    Just (PostEmbedImages ei) -> do
      length (eisImages ei) === 2
      let img0 = eisImages ei !! 0
      eiAlt         img0 === "A beautiful sunset"
      eiAspectRatio img0 === Just (AspectRatio 1920 1080)
      let img1 = eisImages ei !! 1
      eiAlt         img1 === "A bright sunrise"
      eiAspectRatio img1 === Nothing
    _ -> failure

-- | A post with an external embed decodes all fields including thumb.
prop_parseExternalPost :: Property
prop_parseExternalPost = withTests 1 . property $ do
  post <- evalEither (decode feedPostCodec externalPost)
  case fpEmbed post of
    Just (PostEmbedExternal eext) -> do
      let ext = eextExternal eext
      eeUri         ext === "https://example.com/article"
      eeTitle       ext === "An interesting article"
      eeDescription ext === "This is the article description."
      case eeThumb ext of
        Nothing -> failure
        Just _  -> success
    _ -> failure

-- | A post with a record (quote) embed decodes the strong ref.
prop_parseRecordPost :: Property
prop_parseRecordPost = withTests 1 . property $ do
  post <- evalEither (decode feedPostCodec recordPost)
  case fpEmbed post of
    Just (PostEmbedRecord er) -> do
      srUri (erecRecord er) === "at://did:plc:quoted/app.bsky.feed.post/rkey"
      srCid (erecRecord er) === "bafyreiquoted"
    _ -> failure

-- | A post with langs and tags decodes both arrays correctly.
prop_parseLangsTagsPost :: Property
prop_parseLangsTagsPost = withTests 1 . property $ do
  post <- evalEither (decode feedPostCodec langsTagsPost)
  fpLangs post === Just ["en"]
  fpTags  post === Just ["haskell", "atproto"]

-- | A post with self-labels decodes the label value.
prop_parseLabelsPost :: Property
prop_parseLabelsPost = withTests 1 . property $ do
  post <- evalEither (decode feedPostCodec labelsPost)
  case fpLabels post of
    Just (PostLabelsSelf sl) -> do
      length (slValues sl) === 1
      slvVal (slValues sl !! 0) === "!adult"
    Nothing -> failure

-- | Missing 'text' field causes a parse error.
prop_missingTextFails :: Property
prop_missingTextFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"createdAt\": \"2024-01-15T12:00:00.000Z\"}"
  case decode feedPostCodec bad of
    Left  _ -> success
    Right _ -> failure

-- | Missing 'createdAt' field causes a parse error.
prop_missingCreatedAtFails :: Property
prop_missingCreatedAtFails = withTests 1 . property $ do
  let bad = BLC.pack "{\"text\": \"hello\"}"
  case decode feedPostCodec bad of
    Left  _ -> success
    Right _ -> failure

-- | Encoding a 'FeedPost' and decoding the result yields the original value.
prop_roundTrip :: Property
prop_roundTrip = withTests 1 . property $ do
  let original = FeedPost
        { fpText      = "Round-trip test"
        , fpCreatedAt = "2024-06-01T00:00:00.000Z"
        , fpReply     = Nothing
        , fpFacets    = Just
            [ Facet
                { facIndex    = ByteSlice 0 14
                , facFeatures = [FacetFeatureTag (FacetTag "roundtrip")]
                }
            ]
        , fpEmbed     = Nothing
        , fpLangs     = Just ["en"]
        , fpLabels    = Nothing
        , fpTags      = Just ["roundtrip"]
        }
      encoded = encode feedPostCodec original
  decoded <- evalEither (decode feedPostCodec encoded)
  decoded === original

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Bsky.Feed.Post"
  [ ("parse minimal post",              prop_parseMinimalPost)
  , ("parse reply post",                prop_parseReplyPost)
  , ("parse facet post",                prop_parseFacetPost)
  , ("parse images embed",              prop_parseImagesPost)
  , ("parse external embed",            prop_parseExternalPost)
  , ("parse record embed",              prop_parseRecordPost)
  , ("parse langs and tags",            prop_parseLangsTagsPost)
  , ("parse self-labels",               prop_parseLabelsPost)
  , ("missing 'text' fails",            prop_missingTextFails)
  , ("missing 'createdAt' fails",       prop_missingCreatedAtFails)
  , ("round-trip encode/decode",        prop_roundTrip)
  ]
