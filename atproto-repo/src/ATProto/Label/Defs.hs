-- | Typed bindings for @com.atproto.label.defs@.
--
-- Metadata labels used across the AT Protocol for content moderation and
-- self-labelling.
--
-- The Lexicon for these types is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/label/defs.json>.
module ATProto.Label.Defs
  ( -- * Types
    Label (..)
  , SelfLabels (..)
  , SelfLabel (..)
    -- * Codecs
  , labelCodec
  , selfLabelsCodec
  , selfLabelCodec
  ) where

import           Data.Int                (Int64)
import qualified Data.ByteString         as BS
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Schema      as Codec

-- ---------------------------------------------------------------------------
-- Label
-- ---------------------------------------------------------------------------

-- | Metadata tag on an atproto resource (eg, repo or record).
--
-- Corresponds to @com.atproto.label.defs#label@.
data Label = Label
  { labVer :: Maybe Int64
    -- ^ The AT Protocol version of the label object.
  , labSrc :: T.Text
    -- ^ DID of the actor who created this label.
  , labUri :: T.Text
    -- ^ AT URI of the record, repository (account), or other resource
    -- that this label applies to.
  , labCid :: Maybe T.Text
    -- ^ Optionally, CID specifying the specific version of 'uri' resource
    -- this label applies to.
  , labVal :: T.Text
    -- ^ The short string name of the value or type of this label.
  , labNeg :: Maybe Bool
    -- ^ If true, this is a negation label, overwriting a previous label.
  , labCts :: T.Text
    -- ^ Timestamp when this label was created.
  , labExp :: Maybe T.Text
    -- ^ Timestamp at which this label expires (no longer applies).
  , labSig :: Maybe BS.ByteString
    -- ^ Signature of dag-cbor encoded label.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.label.defs#label@.
labelCodec :: Codec Label
labelCodec =
    Codec.record "com.atproto.label.defs#label" $
        Label
            <$> Codec.optionalField "ver" Codec.int                          labVer
            <*> Codec.requiredField "src" Codec.did                          labSrc
            <*> Codec.requiredField "uri" Codec.uri                          labUri
            <*> Codec.optionalField "cid" (Codec.string Codec.LexFormatCid)  labCid
            <*> Codec.requiredField "val" Codec.text                         labVal
            <*> Codec.optionalField "neg" Codec.bool                         labNeg
            <*> Codec.requiredField "cts" Codec.datetime                     labCts
            <*> Codec.optionalField "exp" Codec.datetime                     labExp
            <*> Codec.optionalField "sig" Codec.bytes                        labSig

-- ---------------------------------------------------------------------------
-- SelfLabels / SelfLabel
-- ---------------------------------------------------------------------------

-- | Metadata tags on an atproto record, published by the author within the
-- record.
--
-- Corresponds to @com.atproto.label.defs#selfLabels@.
newtype SelfLabels = SelfLabels
  { slValues :: [SelfLabel]
  } deriving (Eq, Show)

-- | Codec for @com.atproto.label.defs#selfLabels@.
selfLabelsCodec :: Codec SelfLabels
selfLabelsCodec =
    Codec.record "com.atproto.label.defs#selfLabels" $
        SelfLabels
            <$> Codec.requiredField "values" (Codec.array selfLabelCodec) slValues

-- | A single self-label value.
--
-- Corresponds to @com.atproto.label.defs#selfLabel@.
newtype SelfLabel = SelfLabel
  { slVal :: T.Text
    -- ^ The short string name of the value or type of this label.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.label.defs#selfLabel@.
selfLabelCodec :: Codec SelfLabel
selfLabelCodec =
    Codec.record "com.atproto.label.defs#selfLabel" $
        SelfLabel
            <$> Codec.requiredField "val" Codec.text slVal
