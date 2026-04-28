module Main (main) where

import Hedgehog    (checkParallel)
import System.Exit (exitFailure, exitSuccess)

import qualified Test.ATProto.Bsky.Feed.Like          as FeedLike
import qualified Test.ATProto.Bsky.Feed.Post           as FeedPost
import qualified Test.ATProto.Identity.ResolveHandle   as ResolveHandle
import qualified Test.ATProto.Label.Defs               as LabelDefs
import qualified Test.ATProto.Repo.ApplyWrites         as ApplyWrites
import qualified Test.ATProto.Repo.GetProfile          as GetProfile
import qualified Test.ATProto.Repo.ListRecords         as ListRecords
import qualified Test.ATProto.Repo.UploadBlob          as UploadBlob
import qualified Test.ATProto.Server.CreateSession     as CreateSession
import qualified Test.ATProto.Server.DescribeServer    as DescribeServer
import qualified Test.ATProto.Sync.ListRepos           as ListRepos

main :: IO ()
main = do
  results <- mapM checkParallel
    [ FeedLike.tests
    , FeedPost.tests
    , ListRecords.tests
    , GetProfile.tests
    , UploadBlob.tests
    , DescribeServer.tests
    , CreateSession.tests
    , ResolveHandle.tests
    , ListRepos.tests
    , ApplyWrites.tests
    , LabelDefs.tests
    ]
  if and results then exitSuccess else exitFailure
