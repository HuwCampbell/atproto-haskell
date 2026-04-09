-- | Feed generator example – entry point.
--
-- Reads configuration from environment variables, builds the XRPC server,
-- and runs it with Warp.
--
-- Configuration (environment variables with defaults):
--
-- * @SERVICE_DID@ – the DID of this feed generator service
--   (default: @did:web:feed.example.com@)
-- * @FEED_URI@ – the AT-URI of the feed this generator serves
--   (default: @at:\/\/did:web:feed.example.com\/app.bsky.feed.generator\/my-feed@)
-- * @PORT@ – TCP port to listen on (default: @3000@)
--
-- Run with:
--
-- @
-- cabal run feed-generator
-- @
module Main (main) where

import           Control.Monad.Trans.Reader    (runReaderT)
import qualified Data.Aeson                    as Aeson
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Network.HTTP.Types            (status404, hContentType)
import           Network.Wai                   (Application, responseLBS)
import qualified Network.Wai.Handler.Warp      as Warp
import           System.Environment            (lookupEnv)

import           ATProto.DID                   (defaultDidResolver)
import           ATProto.Syntax.NSID           (parseNSID)
import           ATProto.XRPC.Server           (makeServer, query, withAuthVerifier)
import           ATProto.XRPC.Server.Wai       (xrpcMiddleware)

import           FeedGenerator.Auth            (makeServiceAuthVerifier)
import           FeedGenerator.Feed            (handleGetFeedSkeleton)
import           FeedGenerator.Types           (AppEnv (..))

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  -- Read configuration from the environment.
  serviceDid <- fmap (maybe "did:web:feed.example.com" T.pack)
                  (lookupEnv "SERVICE_DID")
  feedUri    <- fmap (maybe defaultFeedUri T.pack)
                  (lookupEnv "FEED_URI")
  port       <- fmap (maybe 3000 read) (lookupEnv "PORT")

  TIO.putStrLn $ "Service DID : " <> serviceDid
  TIO.putStrLn $ "Feed URI    : " <> feedUri

  -- Build the DID resolver (handles both did:plc and did:web, with caching).
  resolver <- defaultDidResolver

  let env = AppEnv
        { envServiceDid  = serviceDid
        , envFeedUri     = feedUri
        , envDidResolver = resolver
        }

  -- Register the getFeedSkeleton endpoint.
  let nsid = case parseNSID "app.bsky.feed.getFeedSkeleton" of
               Right n -> n
               Left  e -> error ("Invalid NSID (this is a bug): " ++ show e)
      server     = withAuthVerifier (makeServiceAuthVerifier env)
                     (makeServer [query nsid handleGetFeedSkeleton])

  -- Build the WAI application: XRPC middleware with a 404 fallback.
  let app = xrpcMiddleware (flip runReaderT env) server notFoundApp

  TIO.putStrLn $ "Feed generator listening on port " <> T.pack (show port)
  Warp.run port app

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Default feed URI used when @FEED_URI@ is not set.
defaultFeedUri :: T.Text
defaultFeedUri = "at://did:web:feed.example.com/app.bsky.feed.generator/my-feed"

-- | Fallback WAI application for non-XRPC paths.
notFoundApp :: Application
notFoundApp _req respond =
  respond $ responseLBS status404
    [(hContentType, "application/json")]
    (Aeson.encode (Aeson.object ["error" Aeson..= ("NotFound" :: T.Text)]))
