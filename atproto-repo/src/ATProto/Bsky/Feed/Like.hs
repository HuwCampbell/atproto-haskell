{-# LANGUAGE LambdaCase #-}
-- | Typed codec for @app.bsky.feed.like@.
--
-- The Lexicon definition used as the basis for this module is at:
--
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/feed/like.json>
--
module ATProto.Bsky.Feed.Like
  ( -- * Feed like (top-level record)
    FeedLike (..)
  , feedLikeCodec
  ) where

import qualified Data.Text              as T
import           ATProto.Lex.Codec      (Codec)
import qualified ATProto.Lex.Codec      as Codec
import           ATProto.Bsky.Feed.Post (StrongRef, strongRefCodec)

-- | A Bluesky like record.
--
-- Defined in @app.bsky.feed.like@.
data FeedLike = FeedLike
  { flSubject   :: StrongRef
    -- ^ The post (or other record) being liked.
  , flCreatedAt :: T.Text
    -- ^ RFC 3339 creation timestamp.
  , flVia       :: Maybe StrongRef
    -- ^ Optional record through which the like was performed (e.g. a repost).
  } deriving (Eq, Show)

-- | Codec for 'FeedLike'.
feedLikeCodec :: Codec FeedLike
feedLikeCodec =
    Codec.record "app.bsky.feed.like" $
        FeedLike
            <$> Codec.requiredField "subject"   strongRefCodec          flSubject
            <*> Codec.requiredField "createdAt" Codec.datetime           flCreatedAt
            <*> Codec.optionalField "via"       strongRefCodec           flVia
