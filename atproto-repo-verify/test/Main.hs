module Main (main) where

import Hedgehog      (checkParallel)
import System.Exit   (exitFailure, exitSuccess)

import qualified Test.ATProto.Repo.Verify.Key    as Key
import qualified Test.ATProto.Repo.Verify.Commit as Commit

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Key.tests
    , Commit.tests
    ]
  if and results then exitSuccess else exitFailure
