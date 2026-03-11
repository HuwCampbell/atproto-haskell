module Main (main) where

import Hedgehog (checkParallel)

import Test.ATProto.OAuth.PKCE  (tests)
import Test.ATProto.OAuth.DPoP  (tests)
import Test.ATProto.OAuth.Types (tests)

main :: IO ()
main = do
  ok1 <- checkParallel Test.ATProto.OAuth.PKCE.tests
  ok2 <- checkParallel Test.ATProto.OAuth.DPoP.tests
  ok3 <- checkParallel Test.ATProto.OAuth.Types.tests
  if ok1 && ok2 && ok3
    then return ()
    else error "Some tests failed"
