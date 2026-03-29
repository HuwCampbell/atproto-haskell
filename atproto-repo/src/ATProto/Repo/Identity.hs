-- | Typed bindings for @com.atproto.identity.*@ XRPC methods.
module ATProto.Repo.Identity
  ( -- * @com.atproto.identity.resolveHandle@
    ResolveHandleResponse (..)
  , resolveHandleResponseCodec
  ) where

import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- | Response from @com.atproto.identity.resolveHandle@.
data ResolveHandleResponse = ResolveHandleResponse
  { rhrDid :: T.Text
  } deriving (Eq, Show)

resolveHandleResponseCodec :: Codec ResolveHandleResponse
resolveHandleResponseCodec =
    Codec.record "com.atproto.identity.resolveHandle#output" $
        ResolveHandleResponse
            <$> Codec.requiredField "did" Codec.did rhrDid
