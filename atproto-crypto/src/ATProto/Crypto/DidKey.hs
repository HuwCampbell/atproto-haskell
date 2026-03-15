-- | @did:key@ encoding and decoding.
--
-- A @did:key@ DID is formed by prepending @\"did:key:\"@ to a multikey
-- string:
--
-- @
-- did:key:z\<base58btc(codec_prefix || compressed_pubkey)\>
-- @
--
-- Reference: <https://atproto.com/specs/did#did-key>
module ATProto.Crypto.DidKey
  ( pubKeyToDidKey
  , didKeyToPubKey
  ) where

import ATProto.Crypto.Multikey
import ATProto.Crypto.Types

-- | The fixed @did:key:@ URI scheme prefix.
didKeyPrefix :: String
didKeyPrefix = "did:key:"

-- | Encode a 'PubKey' as a @did:key@ string.
pubKeyToDidKey :: PubKey -> String
pubKeyToDidKey = (didKeyPrefix <>) . encodeMultikey

-- | Decode a @did:key@ string into a 'PubKey'.
--
-- Returns 'Left' if the input does not start with @\"did:key:\"@ or if the
-- multikey payload is malformed.
didKeyToPubKey :: String -> Either String PubKey
didKeyToPubKey s =
  case stripPrefix didKeyPrefix s of
    Nothing   -> Left "didKeyToPubKey: missing \"did:key:\" prefix"
    Just rest -> decodeMultikey rest
  where
    stripPrefix []     ys     = Just ys
    stripPrefix _      []     = Nothing
    stripPrefix (x:xs) (y:ys)
      | x == y    = stripPrefix xs ys
      | otherwise = Nothing
