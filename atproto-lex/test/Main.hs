module Main (main) where

import Hedgehog        (checkParallel)
import System.Exit     (exitFailure, exitSuccess)

import qualified Test.ATProto.Lex.Codec    as LexCodec
import qualified Test.ATProto.Lex.Validate as LexValidate

main :: IO ()
main = do
  results <- mapM checkParallel [LexCodec.tests, LexValidate.tests]
  if and results then exitSuccess else exitFailure
