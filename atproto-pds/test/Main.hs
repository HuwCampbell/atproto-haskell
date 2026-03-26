module Main (main) where

import Hedgehog      (checkParallel)
import System.Exit   (exitFailure, exitSuccess)

import qualified Test.ATProto.PDS.Repo    as Repo
import qualified Test.ATProto.PDS.Storage as Storage

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Repo.tests
    , Storage.tests
    ]
  if and results then exitSuccess else exitFailure
