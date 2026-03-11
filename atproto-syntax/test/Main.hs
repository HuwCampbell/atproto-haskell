module Main (main) where

import Hedgehog (checkParallel)
import System.Exit (exitFailure, exitSuccess)

import qualified Test.ATProto.Syntax.DID       as DID
import qualified Test.ATProto.Syntax.Handle     as Handle
import qualified Test.ATProto.Syntax.NSID       as NSID
import qualified Test.ATProto.Syntax.AtUri      as AtUri
import qualified Test.ATProto.Syntax.TID        as TID
import qualified Test.ATProto.Syntax.RecordKey  as RecordKey

main :: IO ()
main = do
  results <- mapM checkParallel
    [ DID.tests
    , Handle.tests
    , NSID.tests
    , AtUri.tests
    , TID.tests
    , RecordKey.tests
    ]
  if and results then exitSuccess else exitFailure
