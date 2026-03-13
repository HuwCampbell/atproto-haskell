module Main (main) where

import Hedgehog        (checkParallel)
import System.Exit     (exitFailure, exitSuccess)

import qualified Test.ATProto.Lex.Codec as LexCodec

main :: IO ()
main = do
  results <- mapM checkParallel [LexCodec.tests]
  if and results then exitSuccess else exitFailure
