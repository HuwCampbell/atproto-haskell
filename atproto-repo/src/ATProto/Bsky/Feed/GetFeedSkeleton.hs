-- | Typed codec for @app.bsky.feed.getFeedSkeleton@ and its sub-types.
--
-- This module provides Haskell data types and 'Codec' values for the
-- @app.bsky.feed.getFeedSkeleton@ lexicon, which is used by feed generators
-- to return a skeleton of posts for a given feed.
--
-- The Lexicon definition used as the basis for this module is at:
--
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/feed/getFeedSkeleton.json>
--
module ATProto.Bsky.Feed.GetFeedSkeleton
  ( -- * Reason types
    SkeletonReasonRepost (..)
  , skeletonReasonRepostCodec
  , SkeletonReasonPin (..)
  , skeletonReasonPinCodec
  , SkeletonReason (..)
  , skeletonReasonCodec

    -- * Feed item
  , SkeletonFeedItem (..)
  , skeletonFeedItemCodec

    -- * Output (response body)
  , GetFeedSkeletonOutput (..)
  , getFeedSkeletonOutputCodec
  ) where

import qualified Data.Text          as T

import           ATProto.Lex.Codec  (Codec)
import qualified ATProto.Lex.Codec  as Codec

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A repost reason for a skeleton feed item.
--
-- Defined in @app.bsky.feed.defs#skeletonReasonRepost@.
data SkeletonReasonRepost = SkeletonReasonRepost
  { srrRepost :: T.Text
    -- ^ AT-URI of the repost record.
  } deriving (Eq, Show)

-- | A pin reason for a skeleton feed item (empty object).
--
-- Defined in @app.bsky.feed.defs#skeletonReasonPin@.
data SkeletonReasonPin = SkeletonReasonPin
  deriving (Eq, Show)

-- | Union of all skeleton reason types.
data SkeletonReason
  = SkeletonReasonIsRepost SkeletonReasonRepost
    -- ^ @app.bsky.feed.defs#skeletonReasonRepost@
  | SkeletonReasonIsPin SkeletonReasonPin
    -- ^ @app.bsky.feed.defs#skeletonReasonPin@
  deriving (Eq, Show)

-- | A single item in a feed skeleton.
--
-- Defined in @app.bsky.feed.defs#skeletonFeedItem@.
data SkeletonFeedItem = SkeletonFeedItem
  { sfiPost        :: T.Text
    -- ^ AT-URI of the post.
  , sfiReason      :: Maybe SkeletonReason
    -- ^ Optional reason this item appears in the feed.
  , sfiFeedContext :: Maybe T.Text
    -- ^ Opaque per-item context string (max 2000 chars), passed back to the
    -- feed generator during interactions.
  } deriving (Eq, Show)

-- | The response body for @app.bsky.feed.getFeedSkeleton@.
data GetFeedSkeletonOutput = GetFeedSkeletonOutput
  { gfsCursor :: Maybe T.Text
    -- ^ Pagination cursor for the next page.
  , gfsFeed   :: [SkeletonFeedItem]
    -- ^ List of skeleton feed items.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Codecs
-- ---------------------------------------------------------------------------

-- | Codec for 'SkeletonReasonRepost'.
skeletonReasonRepostCodec :: Codec SkeletonReasonRepost
skeletonReasonRepostCodec =
    Codec.record "app.bsky.feed.defs#skeletonReasonRepost" $
        SkeletonReasonRepost
            <$> Codec.requiredField "repost" Codec.atUri srrRepost

-- | Codec for 'SkeletonReasonPin'.
skeletonReasonPinCodec :: Codec SkeletonReasonPin
skeletonReasonPinCodec =
    Codec.record "app.bsky.feed.defs#skeletonReasonPin" $
        pure SkeletonReasonPin

-- | Codec for the 'SkeletonReason' union.
skeletonReasonCodec :: Codec SkeletonReason
skeletonReasonCodec =
  let
    from (SkeletonReasonIsRepost r) = Left r
    from (SkeletonReasonIsPin p)    = Right p

    to (Left r)  = SkeletonReasonIsRepost r
    to (Right p) = SkeletonReasonIsPin p
  in
  Codec.invmap to from $
    Codec.union skeletonReasonRepostCodec skeletonReasonPinCodec

-- | Codec for 'SkeletonFeedItem'.
skeletonFeedItemCodec :: Codec SkeletonFeedItem
skeletonFeedItemCodec =
    Codec.record "app.bsky.feed.defs#skeletonFeedItem" $
        SkeletonFeedItem
            <$> Codec.requiredField "post"        Codec.atUri                      sfiPost
            <*> Codec.optionalField "reason"      skeletonReasonCodec              sfiReason
            <*> Codec.optionalField "feedContext" Codec.text                       sfiFeedContext

-- | Codec for 'GetFeedSkeletonOutput'.
getFeedSkeletonOutputCodec :: Codec GetFeedSkeletonOutput
getFeedSkeletonOutputCodec =
    Codec.record "app.bsky.feed.getFeedSkeleton#output" $
        GetFeedSkeletonOutput
            <$> Codec.optionalField "cursor" Codec.text                           gfsCursor
            <*> Codec.requiredField "feed"   (Codec.array skeletonFeedItemCodec)  gfsFeed
