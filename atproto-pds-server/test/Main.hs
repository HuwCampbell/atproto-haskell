module Main (main) where

import System.Exit (exitFailure, exitSuccess)

import qualified Hedgehog

import qualified Test.ATProto.PDS.Server

main :: IO ()
main = do
  ok <- Hedgehog.checkSequential Test.ATProto.PDS.Server.tests
  if ok then exitSuccess else exitFailure
