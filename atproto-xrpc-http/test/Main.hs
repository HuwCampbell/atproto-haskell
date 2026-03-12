module Main (main) where

import Hedgehog    (checkParallel)
import System.Exit (exitFailure, exitSuccess)

import qualified Test.ATProto.XRPC.Http as Http

main :: IO ()
main = do
  results <- mapM checkParallel [Http.tests]
  if and results then exitSuccess else exitFailure
