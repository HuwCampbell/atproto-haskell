module Main (main) where

import Hedgehog        (checkParallel)
import System.Exit     (exitFailure, exitSuccess)

main :: IO ()
main = do
  results <- mapM checkParallel []
  if and results then exitSuccess else exitFailure
