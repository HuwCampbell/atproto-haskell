-- | Extract the @#atproto@ signing key from a DID document.
module ATProto.Repo.Verify.Key
  ( resolveAtprotoKey
  ) where

import qualified Data.Text as T

import ATProto.DID.Document       (DidDocument (..), VerificationMethod (..))
import ATProto.Crypto.Types       (PubKey)
import ATProto.Crypto.Multikey    (decodeMultikey)
import ATProto.Repo.Verify.Types  (VerifyError (..))

-- | Find the @#atproto@ 'VerificationMethod' in the DID document and decode
-- its public key.
--
-- Returns 'VerifyNoAtprotoKey' if no matching method is found, or
-- 'VerifyBadKey' if the multikey cannot be decoded.
resolveAtprotoKey :: DidDocument -> Either VerifyError PubKey
resolveAtprotoKey doc =
  case filter isAtprotoMethod (didDocVerificationMethods doc) of
    []     -> Left VerifyNoAtprotoKey
    (vm:_) ->
      case vmPublicKeyMultibase vm of
        Nothing  -> Left VerifyNoAtprotoKey
        Just mk  ->
          case decodeMultikey (T.unpack mk) of
            Left err -> Left (VerifyBadKey err)
            Right pk -> Right pk
  where
    isAtprotoMethod vm = "#atproto" `T.isSuffixOf` vmId vm
