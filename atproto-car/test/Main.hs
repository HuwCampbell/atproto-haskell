module Main (main) where

import Hedgehog      (checkParallel)
import System.Exit   (exitFailure, exitSuccess)

import qualified Test.ATProto.Car.Cid    as Cid
import qualified Test.ATProto.Car.Parser as Parser
import qualified Test.ATProto.Car.Writer as Writer

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Cid.tests
    , Parser.tests
    , Writer.tests
    ]
  if and results then exitSuccess else exitFailure
