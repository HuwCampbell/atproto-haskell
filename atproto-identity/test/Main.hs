module Main (main) where

import Hedgehog    (checkParallel)
import System.Exit (exitFailure, exitSuccess)

import qualified Test.ATProto.Identity.Handle as Handle

main :: IO ()
main = do
  results <- mapM checkParallel [Handle.tests]
  if and results then exitSuccess else exitFailure
