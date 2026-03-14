module Main (main) where

import Hedgehog    (checkParallel)
import System.Exit (exitFailure, exitSuccess)

import qualified Test.ATProto.Bsky.Feed.Post   as FeedPost
import qualified Test.ATProto.Repo.GetProfile  as GetProfile
import qualified Test.ATProto.Repo.ListRecords as ListRecords

main :: IO ()
main = do
  results <- mapM checkParallel [FeedPost.tests, ListRecords.tests, GetProfile.tests]
  if and results then exitSuccess else exitFailure
