-- | Typed bindings for @com.atproto.repo.applyWrites@.
module ATProto.Repo.ApplyWrites
  ( -- * Request
    ApplyWritesRequest (..)
  , applyWritesRequestCodec
  ) where

import qualified Data.Text               as T

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- | Input body for @com.atproto.repo.applyWrites@.
data ApplyWritesRequest = ApplyWritesRequest
  { awrRepo   :: T.Text
    -- ^ The DID of the repository.
  , awrWrites :: [LexValue]
    -- ^ The write operations (create\/update\/delete objects).
  } deriving (Eq, Show)

applyWritesRequestCodec :: Codec ApplyWritesRequest
applyWritesRequestCodec =
    Codec.record "com.atproto.repo.applyWrites" $
        ApplyWritesRequest
            <$> Codec.requiredField "repo"   Codec.did                    awrRepo
            <*> Codec.requiredField "writes" (Codec.array Codec.lexValue) awrWrites
