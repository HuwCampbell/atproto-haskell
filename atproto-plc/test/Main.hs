module Main (main) where

import Hedgehog      (checkParallel)
import System.Exit   (exitFailure, exitSuccess)

import qualified Test.ATProto.PLC.Types as Types

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Types.tests
    ]
  if and results then exitSuccess else exitFailure
