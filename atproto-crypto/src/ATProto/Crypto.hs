-- | AT Protocol cryptographic operations.
--
-- This module provides the public API for the two elliptic curves used by
-- the AT Protocol – P-256 and secp256k1 – along with the @did:key@ encoding
-- used to represent public keys as decentralised identifiers.
--
-- = Typical usage
--
-- == Generating a keypair and signing a message
--
-- @
-- import ATProto.Crypto
-- import qualified Data.ByteString.Char8 as BC
--
-- main :: IO ()
-- main = do
--   -- Generate a fresh P-256 keypair using the OS CSPRNG.
--   (priv, pub) <- generateKeyPair P256
--
--   -- Sign a message.  SHA-256 is applied internally; the result is a
--   -- 64-byte compact (r || s) signature with low-S enforced.
--   let msg = BC.pack "hello atproto"
--   sig <- sign priv msg
--
--   -- Verify the signature in Strict mode (rejects high-S / malleable sigs).
--   print (verify Strict pub msg sig)   -- True
-- @
--
-- == Publishing a public key as a did:key
--
-- @
--   let did = pubKeyToDidKey pub
--   -- did:key:zDnae...
--   putStrLn did
--
--   -- Round-trip back to the PubKey.
--   case didKeyToPubKey did of
--     Left  err -> putStrLn ("Parse error: " ++ err)
--     Right pk  -> print (pk == pub)   -- True
-- @
--
-- == Importing an existing private key
--
-- @
--   import qualified Data.ByteString as BS
--
--   -- Load 32 raw bytes from secure storage.
--   let rawKey = BS.replicate 32 0x01   -- example only – never use zeroes!
--   case importPrivKey Secp256k1 rawKey of
--     Left  err  -> putStrLn ("Invalid key: " ++ err)
--     Right priv -> do
--       let pub = derivePublicKey priv
--       putStrLn (pubKeyToDidKey pub)
-- @
module ATProto.Crypto
  ( -- * Curves
    Curve (..)
  , jwtAlg
    -- * Signature strictness
  , SigStrictness (..)
    -- * Key types
  , PrivKey (..)
  , PubKey (..)
    -- * Signatures
  , Signature (..)
    -- * Key management
  , generateKeyPair
  , importPrivKey
  , derivePublicKey
    -- * Signing and verification
  , sign
  , verify
    -- * @did:key@ encoding
  , pubKeyToDidKey
  , didKeyToPubKey
  ) where

import ATProto.Crypto.Types
import ATProto.Crypto.EC
import ATProto.Crypto.DidKey
