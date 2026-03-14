module Main (main) where

import Hedgehog        (checkParallel)
import System.Exit     (exitFailure, exitSuccess)

import qualified Test.ATProto.ServiceAuth.Roundtrip as Roundtrip

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Roundtrip.tests
    ]
  if and results then exitSuccess else exitFailure
