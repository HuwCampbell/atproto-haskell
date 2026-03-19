-- | Typed binding for @com.atproto.repo.defs#commitMeta@.
--
-- The Lexicon for this type is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/defs.json>.
module ATProto.Repo.CommitMeta
  ( -- * Types
    CommitMeta (..)
    -- * Codec
  , commitMetaCodec
  ) where

import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Schema as Codec

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Type for @com.atproto.repo.defs#commitMeta@.
data CommitMeta = CommitMeta
  { cmCid         :: T.Text
    -- ^ Commit CID
  , cmRev         :: T.Text
    -- ^ Revision TID
  } deriving (Eq, Show)


-- | Codec for the @com.atproto.repo.defs#commitMeta@ response body.
commitMetaCodec :: Codec CommitMeta
commitMetaCodec =
    Codec.record "com.atproto.repo.defs#commitMeta" $
        CommitMeta
            <$> Codec.requiredField "cid"  (Codec.string Codec.LexFormatCid)   cmCid
            <*> Codec.requiredField "rev"  (Codec.string Codec.LexFormatTid)   cmRev
