module Main (main) where

import Hedgehog      (checkParallel)
import System.Exit   (exitFailure, exitSuccess)

import qualified Test.ATProto.MST.Layer as Layer
import qualified Test.ATProto.MST.Tree  as Tree
import qualified Test.ATProto.MST.Build as Build

main :: IO ()
main = do
  results <- mapM checkParallel
    [ Layer.tests
    , Tree.tests
    , Build.tests
    ]
  if and results then exitSuccess else exitFailure
