module Main (main) where

import Hedgehog (checkParallel)

import Test.ATProto.OAuth.Provider.DPoP.Nonce    (tests)
import Test.ATProto.OAuth.Provider.DPoP.Verifier  (tests)
import Test.ATProto.OAuth.Provider.Token           (tests)
import Test.ATProto.OAuth.Provider.Verifier        (tests)

main :: IO ()
main = do
  ok1 <- checkParallel Test.ATProto.OAuth.Provider.DPoP.Nonce.tests
  ok2 <- checkParallel Test.ATProto.OAuth.Provider.DPoP.Verifier.tests
  ok3 <- checkParallel Test.ATProto.OAuth.Provider.Token.tests
  ok4 <- checkParallel Test.ATProto.OAuth.Provider.Verifier.tests
  if ok1 && ok2 && ok3 && ok4
    then return ()
    else error "Some tests failed"
