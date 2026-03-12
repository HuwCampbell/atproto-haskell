module Main (main) where

import Hedgehog    (checkParallel)
import System.Exit (exitFailure, exitSuccess)

import qualified Test.ATProto.Lexicon.Json as Json

main :: IO ()
main = do
  results <- mapM checkParallel [Json.tests]
  if and results then exitSuccess else exitFailure
