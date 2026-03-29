-- | Typed bindings for @com.atproto.repo.describeRepo@.
module ATProto.Repo.DescribeRepo
  ( -- * Response
    DescribeRepoResponse (..)
  , describeRepoResponseCodec
  ) where

import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- | Response from @com.atproto.repo.describeRepo@.
data DescribeRepoResponse = DescribeRepoResponse
  { drHandle          :: T.Text
  , drDid             :: T.Text
  , drHandleIsCorrect :: Bool
  , drCollections     :: [T.Text]
  } deriving (Eq, Show)

describeRepoResponseCodec :: Codec DescribeRepoResponse
describeRepoResponseCodec =
    Codec.record "com.atproto.repo.describeRepo#output" $
        DescribeRepoResponse
            <$> Codec.requiredField "handle"          Codec.handle         drHandle
            <*> Codec.requiredField "did"             Codec.did            drDid
            <*> Codec.requiredField "handleIsCorrect" Codec.bool           drHandleIsCorrect
            <*> Codec.requiredField "collections"     (Codec.array Codec.text) drCollections
