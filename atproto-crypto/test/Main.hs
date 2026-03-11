module Main (main) where

import Hedgehog        (checkParallel)
import System.Exit     (exitFailure, exitSuccess)

import qualified Test.ATProto.Crypto.Base58  as Base58
import qualified Test.ATProto.Crypto.EC      as EC
import qualified Test.ATProto.Crypto.DidKey  as DidKey

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Base58.tests
    , EC.tests
    , DidKey.tests
    ]
  if and results then exitSuccess else exitFailure
