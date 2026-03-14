module Main (main) where

import Hedgehog      (checkParallel)
import System.Exit   (exitFailure, exitSuccess)

import qualified Test.ATProto.Tap.Events as Events

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Events.tests
    ]
  if and results then exitSuccess else exitFailure
