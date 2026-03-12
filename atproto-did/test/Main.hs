module Main (main) where

import Hedgehog        (checkParallel)
import System.Exit     (exitFailure, exitSuccess)

import qualified Test.ATProto.DID.Document as Document
import qualified Test.ATProto.DID.Resolver as Resolver

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Document.tests
    , Resolver.tests
    ]
  if and results then exitSuccess else exitFailure
