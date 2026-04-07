-- | Feed skeleton handler for the feed generator example.
--
-- Implements @app.bsky.feed.getFeedSkeleton@, returning a stubbed list of
-- posts.  Authentication is required: unauthenticated requests are rejected
-- with @AuthRequired@.  Only the feed URI configured in 'AppEnv' is served;
-- requests for any other feed return @UnknownFeed@.
module FeedGenerator.Feed
  ( handleGetFeedSkeleton
  ) where

import           Control.Monad.Trans.Reader (asks)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T

import           ATProto.Bsky.Feed.GetFeedSkeleton
                                             ( GetFeedSkeletonOutput (..)
                                             , SkeletonFeedItem (..)
                                             , getFeedSkeletonOutputCodec
                                             )
import qualified ATProto.Lex.Json           as Lex
import           ATProto.XRPC.Server.Types  (XrpcHandlerResult (..), XrpcServerRequest (..))

import           FeedGenerator.Types        (AppEnv (..), AppM)

-- | Handle @GET /xrpc/app.bsky.feed.getFeedSkeleton@.
--
-- Steps:
--
-- 1. Require an authenticated caller.
-- 2. Check that the requested @feed@ parameter matches the configured feed URI.
-- 3. Parse optional @limit@ (default 50) and @cursor@ query parameters.
-- 4. Return a stubbed 'GetFeedSkeletonOutput' as JSON.
handleGetFeedSkeleton :: XrpcServerRequest T.Text -> AppM XrpcHandlerResult
handleGetFeedSkeleton req = do
  -- 1. Require authentication.
  case xsrCaller req of
    Nothing ->
      return $ XrpcHandlerError "AuthRequired" (Just "Authenticated requests only")
    Just _callerDid -> do
      feedUri <- asks envFeedUri

      -- 2. Validate the requested feed URI.
      let params = xsrParams req
      case Map.lookup "feed" params of
        Nothing ->
          return $ XrpcHandlerError "InvalidRequest" (Just "Missing required parameter: feed")
        Just reqFeed | reqFeed /= feedUri ->
          return $ XrpcHandlerError "UnknownFeed" (Just "Feed not found")
        Just _ -> do
          -- 3. Parse optional parameters.
          let _limit  = maybe 50 (min 100 . max 1 . readInt) (Map.lookup "limit"  params)
              _cursor = Map.lookup "cursor" params

          -- 4. Return a stubbed feed (empty for this example).
          let output = GetFeedSkeletonOutput
                { gfsCursor = Nothing
                , gfsFeed   = exampleFeedItems
                }
          return $ XrpcSuccess (Lex.encode getFeedSkeletonOutputCodec output)

-- | Example feed items returned by this stub generator.
--
-- Replace this with a real query against your backing store.
exampleFeedItems :: [SkeletonFeedItem]
exampleFeedItems =
  [ SkeletonFeedItem
      { sfiPost        = "at://did:plc:ewvi7nxzyoun6zhhandbv25b/app.bsky.feed.post/3jxtqcfmzzk22"
      , sfiReason      = Nothing
      , sfiFeedContext = Nothing
      }
  ]

-- | Parse a decimal integer from 'T.Text', returning 0 on failure.
readInt :: T.Text -> Int
readInt t =
  case reads (T.unpack t) of
    [(n, "")] -> n
    _         -> 0
