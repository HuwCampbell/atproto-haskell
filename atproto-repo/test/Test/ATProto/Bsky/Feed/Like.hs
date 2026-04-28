module Test.ATProto.Bsky.Feed.Like (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Hedgehog

import ATProto.Lex.Json          (decode, encode)
import ATProto.Bsky.Feed.Like
import ATProto.Bsky.Feed.Post    (StrongRef (..))

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A minimal like with the two required fields.
minimalLike :: BLC.ByteString
minimalLike = BLC.pack $ unwords
  [ "{"
  , "  \"$type\": \"app.bsky.feed.like\","
  , "  \"subject\": { \"uri\": \"at://did:plc:abc/app.bsky.feed.post/rkey\", \"cid\": \"bafyreiexample\" },"
  , "  \"createdAt\": \"2024-01-15T12:00:00.000Z\""
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | A minimal like decodes correctly.
prop_parseMinimalLike :: Property
prop_parseMinimalLike = withTests 1 . property $ do
  fp <- evalEither (decode feedLikeCodec minimalLike)
  flCreatedAt fp               === "2024-01-15T12:00:00.000Z"
  srUri (flSubject fp)         === "at://did:plc:abc/app.bsky.feed.post/rkey"
  srCid (flSubject fp)         === "bafyreiexample"

-- | Encoding a 'FeedLike' and decoding the result yields the original value.
prop_roundTrip :: Property
prop_roundTrip = withTests 1 . property $ do
  fp <- evalEither (decode feedLikeCodec minimalLike)
  decoded <- evalEither (decode feedLikeCodec (encode feedLikeCodec fp))
  decoded === fp

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "Bsky.Feed.Like"
  [ ("parse minimal like",       prop_parseMinimalLike)
  , ("round-trip encode/decode", prop_roundTrip)
  ]
