module Main (main) where

import Hedgehog      (checkParallel)
import System.Exit   (exitFailure, exitSuccess)

import qualified Test.ATProto.Firehose.Frame as Frame

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Frame.tests
    ]
  if and results then exitSuccess else exitFailure
