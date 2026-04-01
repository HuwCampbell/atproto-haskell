{-# LANGUAGE LambdaCase #-}
-- | Typed codec for @app.bsky.feed.post@ and all its sub-types.
--
-- This module provides Haskell data types and 'Codec' values for the
-- @app.bsky.feed.post@ record lexicon, covering every embedded type in the
-- Bluesky post schema: strong references, reply references, richtext facets
-- (mention, link, tag), all five embed variants (images, video, external,
-- record, record-with-media), and the self-labels union.
--
-- The Lexicon definitions used as the basis for this module are at:
--
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/feed/post.json>
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/richtext/facet.json>
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/embed/images.json>
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/embed/video.json>
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/embed/external.json>
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/embed/record.json>
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/embed/recordWithMedia.json>
--   * <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/label/defs.json>
--
module ATProto.Bsky.Feed.Post
  ( -- * Strong reference
    StrongRef (..)
  , strongRefCodec

    -- * Reply reference
  , ReplyRef (..)
  , replyRefCodec

    -- * Richtext facets
  , ByteSlice (..)
  , byteSliceCodec
  , FacetMention (..)
  , facetMentionCodec
  , FacetLink (..)
  , facetLinkCodec
  , FacetTag (..)
  , facetTagCodec
  , FacetFeature (..)
  , facetFeatureCodec
  , Facet (..)
  , facetCodec

    -- * Aspect ratio
  , AspectRatio (..)
  , aspectRatioCodec

    -- * Images embed
  , EmbedImage (..)
  , embedImageCodec
  , EmbedImages (..)
  , embedImagesCodec

    -- * Video embed
  , VideoCaption (..)
  , videoCaptionCodec
  , EmbedVideo (..)
  , embedVideoCodec

    -- * External embed
  , ExternalEmbed (..)
  , externalEmbedCodec
  , EmbedExternal (..)
  , embedExternalCodec

    -- * Record embed
  , EmbedRecord (..)
  , embedRecordCodec

    -- * Record-with-media embed
  , MediaEmbed (..)
  , mediaEmbedCodec
  , EmbedRecordWithMedia (..)
  , embedRecordWithMediaCodec

    -- * Post embed union
  , PostEmbed (..)
  , postEmbedCodec

    -- * Labels
  , SelfLabelValue (..)
  , selfLabelValueCodec
  , SelfLabels (..)
  , selfLabelsCodec
  , PostLabels (..)
  , postLabelsCodec

    -- * Feed post (top-level record)
  , FeedPost (..)
  , feedPostCodec
  ) where

import qualified Data.Text           as T
import           Data.Int            (Int64)

import           ATProto.Ipld.Value  (BlobRef)
import           ATProto.Lex.Codec   (Codec)
import qualified ATProto.Lex.Codec   as Codec
import           ATProto.Lex.Schema  (LexFormat (..))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A strong reference to a specific record version.
--
-- Defined in @com.atproto.repo.strongRef@.
data StrongRef = StrongRef
  { srUri :: T.Text
    -- ^ AT-URI of the record.
  , srCid :: T.Text
    -- ^ CID of the specific version.
  } deriving (Eq, Show)

-- | Reference to the root and parent of a reply thread.
--
-- Defined in @app.bsky.feed.post#replyRef@.
data ReplyRef = ReplyRef
  { rrRoot   :: StrongRef
    -- ^ The root post of the thread.
  , rrParent :: StrongRef
    -- ^ The immediate parent this post is replying to.
  } deriving (Eq, Show)

-- | A byte-offset range within a UTF-8 string.
--
-- Defined in @app.bsky.richtext.facet#byteSlice@.
data ByteSlice = ByteSlice
  { bsByteStart :: Int64
    -- ^ Inclusive start offset (bytes).
  , bsByteEnd   :: Int64
    -- ^ Exclusive end offset (bytes).
  } deriving (Eq, Show)

-- | A mention facet feature — tags a DID in the text.
--
-- Defined in @app.bsky.richtext.facet#mention@.
data FacetMention = FacetMention
  { fmDid :: T.Text
    -- ^ DID of the mentioned account.
  } deriving (Eq, Show)

-- | A link facet feature — attaches a URI to a text range.
--
-- Defined in @app.bsky.richtext.facet#link@.
data FacetLink = FacetLink
  { flUri :: T.Text
    -- ^ The linked URI.
  } deriving (Eq, Show)

-- | A tag facet feature — marks a hashtag in the text.
--
-- Defined in @app.bsky.richtext.facet#tag@.
data FacetTag = FacetTag
  { ftTag :: T.Text
    -- ^ The tag text (without the @#@ prefix).
  } deriving (Eq, Show)

-- | Union of all richtext facet feature types.
data FacetFeature
  = FacetFeatureMention FacetMention
    -- ^ @app.bsky.richtext.facet#mention@
  | FacetFeatureLink    FacetLink
    -- ^ @app.bsky.richtext.facet#link@
  | FacetFeatureTag     FacetTag
    -- ^ @app.bsky.richtext.facet#tag@
  deriving (Eq, Show)

-- | A richtext facet: a byte-range annotation with one or more features.
--
-- Defined in @app.bsky.richtext.facet@.
data Facet = Facet
  { facIndex    :: ByteSlice
    -- ^ The byte range this facet covers.
  , facFeatures :: [FacetFeature]
    -- ^ The features (mention, link, tag) attached to this range.
  } deriving (Eq, Show)

-- | Width and height dimensions, used for images and video.
data AspectRatio = AspectRatio
  { arWidth  :: Int64
    -- ^ Width in pixels.
  , arHeight :: Int64
    -- ^ Height in pixels.
  } deriving (Eq, Show)

-- | A single image inside an images embed.
--
-- Defined in @app.bsky.embed.images#image@.
data EmbedImage = EmbedImage
  { eiImage       :: BlobRef
    -- ^ The image blob.
  , eiAlt         :: T.Text
    -- ^ Alt text for accessibility.
  , eiAspectRatio :: Maybe AspectRatio
    -- ^ Optional display aspect ratio.
  } deriving (Eq, Show)

-- | An embed containing one or more images.
--
-- Defined in @app.bsky.embed.images@.
data EmbedImages = EmbedImages
  { eisImages :: [EmbedImage]
    -- ^ The images (at least one).
  } deriving (Eq, Show)

-- | A caption file for a video embed.
--
-- Defined in @app.bsky.embed.video#caption@.
data VideoCaption = VideoCaption
  { vcLang :: T.Text
    -- ^ BCP-47 language tag.
  , vcFile :: BlobRef
    -- ^ The caption blob.
  } deriving (Eq, Show)

-- | An embed containing a video.
--
-- Defined in @app.bsky.embed.video@.
data EmbedVideo = EmbedVideo
  { evVideo       :: BlobRef
    -- ^ The video blob.
  , evCaptions    :: Maybe [VideoCaption]
    -- ^ Optional list of caption files.
  , evAlt         :: Maybe T.Text
    -- ^ Optional alt text description.
  , evAspectRatio :: Maybe AspectRatio
    -- ^ Optional display aspect ratio.
  } deriving (Eq, Show)

-- | The external link content inside an external embed.
--
-- Defined in @app.bsky.embed.external#external@.
data ExternalEmbed = ExternalEmbed
  { eeUri         :: T.Text
    -- ^ The external URI.
  , eeTitle       :: T.Text
    -- ^ Link card title.
  , eeDescription :: T.Text
    -- ^ Link card description.
  , eeThumb       :: Maybe BlobRef
    -- ^ Optional thumbnail blob.
  } deriving (Eq, Show)

-- | An embed wrapping an external link card.
--
-- Defined in @app.bsky.embed.external@.
data EmbedExternal = EmbedExternal
  { eextExternal :: ExternalEmbed
    -- ^ The external link content.
  } deriving (Eq, Show)

-- | An embed quoting another ATProto record.
--
-- Defined in @app.bsky.embed.record@.
data EmbedRecord = EmbedRecord
  { erecRecord :: StrongRef
    -- ^ Strong reference to the quoted record.
  } deriving (Eq, Show)

-- | The media part of a record-with-media embed.
data MediaEmbed
  = MediaEmbedImages   EmbedImages
    -- ^ @app.bsky.embed.images@
  | MediaEmbedVideo    EmbedVideo
    -- ^ @app.bsky.embed.video@
  | MediaEmbedExternal EmbedExternal
    -- ^ @app.bsky.embed.external@
  deriving (Eq, Show)

-- | An embed combining a quoted record with attached media.
--
-- Defined in @app.bsky.embed.recordWithMedia@.
data EmbedRecordWithMedia = EmbedRecordWithMedia
  { erwmRecord :: EmbedRecord
    -- ^ The quoted record.
  , erwmMedia  :: MediaEmbed
    -- ^ The accompanying media.
  } deriving (Eq, Show)

-- | Union of all embed types that may appear in a post.
data PostEmbed
  = PostEmbedImages          EmbedImages
    -- ^ @app.bsky.embed.images@
  | PostEmbedVideo           EmbedVideo
    -- ^ @app.bsky.embed.video@
  | PostEmbedExternal        EmbedExternal
    -- ^ @app.bsky.embed.external@
  | PostEmbedRecord          EmbedRecord
    -- ^ @app.bsky.embed.record@
  | PostEmbedRecordWithMedia EmbedRecordWithMedia
    -- ^ @app.bsky.embed.recordWithMedia@
  deriving (Eq, Show)

-- | A single self-label value.
--
-- Defined in @com.atproto.label.defs#selfLabelValue@.
data SelfLabelValue = SelfLabelValue
  { slvVal :: T.Text
    -- ^ The label string (e.g. @\"!adult\"@).
  } deriving (Eq, Show)

-- | A collection of self-applied labels.
--
-- Defined in @com.atproto.label.defs#selfLabels@.
data SelfLabels = SelfLabels
  { slValues :: [SelfLabelValue]
    -- ^ The label values.
  } deriving (Eq, Show)

-- | Union of label types that may appear in a post.
newtype PostLabels = PostLabelsSelf SelfLabels
    -- ^ @com.atproto.label.defs#selfLabels@
  deriving (Eq, Show)

-- | A Bluesky post record.
--
-- Defined in @app.bsky.feed.post@.
data FeedPost = FeedPost
  { fpText      :: T.Text
    -- ^ The post text (max 300 graphemes / 3000 bytes).
  , fpCreatedAt :: T.Text
    -- ^ RFC 3339 creation timestamp.
  , fpReply     :: Maybe ReplyRef
    -- ^ Present when this post is a reply.
  , fpFacets    :: Maybe [Facet]
    -- ^ Richtext annotations on the post text.
  , fpEmbed     :: Maybe PostEmbed
    -- ^ Optional embedded content (images, video, link, quote).
  , fpLangs     :: Maybe [T.Text]
    -- ^ BCP-47 language tags declared by the author.
  , fpLabels    :: Maybe PostLabels
    -- ^ Self-applied content labels.
  , fpTags      :: Maybe [T.Text]
    -- ^ Additional hashtags (max 8, 640 bytes each).
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Codecs
-- ---------------------------------------------------------------------------

-- | Codec for 'StrongRef'.
strongRefCodec :: Codec StrongRef
strongRefCodec =
    Codec.namedType "com.atproto.repo.strongRef" $
    Codec.record "com.atproto.repo.strongRef" $
        StrongRef
            <$> Codec.requiredField "uri" Codec.atUri srUri
            <*> Codec.requiredField "cid" Codec.text  srCid

-- | Codec for 'ReplyRef'.
replyRefCodec :: Codec ReplyRef
replyRefCodec =
    Codec.record "app.bsky.feed.post#replyRef" $
        ReplyRef
            <$> Codec.requiredField "root"   strongRefCodec rrRoot
            <*> Codec.requiredField "parent" strongRefCodec rrParent

-- | Codec for 'ByteSlice'.
byteSliceCodec :: Codec ByteSlice
byteSliceCodec =
    Codec.record "app.bsky.richtext.facet#byteSlice" $
        ByteSlice
            <$> Codec.requiredField "byteStart" Codec.int bsByteStart
            <*> Codec.requiredField "byteEnd"   Codec.int bsByteEnd

-- | Codec for 'FacetMention'.
facetMentionCodec :: Codec FacetMention
facetMentionCodec =
    Codec.record "app.bsky.richtext.facet#mention" $
        FacetMention
            <$> Codec.requiredField "did" Codec.did fmDid

-- | Codec for 'FacetLink'.
facetLinkCodec :: Codec FacetLink
facetLinkCodec =
    Codec.record "app.bsky.richtext.facet#link" $
        FacetLink
            <$> Codec.requiredField "uri" Codec.uri flUri

-- | Codec for 'FacetTag'.
facetTagCodec :: Codec FacetTag
facetTagCodec =
    Codec.record "app.bsky.richtext.facet#tag" $
        FacetTag
            <$> Codec.requiredField "tag" Codec.text ftTag

-- | Codec for the 'FacetFeature' union.
facetFeatureCodec :: Codec FacetFeature
facetFeatureCodec =
  let
    from = \case
      FacetFeatureMention a -> Left a
      FacetFeatureLink a -> Right (Left a)
      FacetFeatureTag a -> Right (Right a)

    to = \case
      Left a -> FacetFeatureMention a
      Right (Left a) -> FacetFeatureLink a
      Right (Right a) -> FacetFeatureTag a
  in
  Codec.invmap to from $
    Codec.union3
      facetMentionCodec
      facetLinkCodec
      facetTagCodec

-- | Codec for 'Facet'.
facetCodec :: Codec Facet
facetCodec =
    Codec.record "app.bsky.richtext.facet" $
        Facet
            <$> Codec.requiredField "index"    byteSliceCodec                   facIndex
            <*> Codec.requiredField "features" (Codec.array facetFeatureCodec)  facFeatures

-- | Codec for 'AspectRatio'.
aspectRatioCodec :: Codec AspectRatio
aspectRatioCodec =
    Codec.record "app.bsky.embed.defs#aspectRatio" $
        AspectRatio
            <$> Codec.requiredField "width"  Codec.int arWidth
            <*> Codec.requiredField "height" Codec.int arHeight

-- | Codec for 'EmbedImage'.
embedImageCodec :: Codec EmbedImage
embedImageCodec =
    Codec.record "app.bsky.embed.images#image" $
        EmbedImage
            <$> Codec.requiredField "image"       Codec.blob                eiImage
            <*> Codec.requiredField "alt"         Codec.text                eiAlt
            <*> Codec.optionalField "aspectRatio" aspectRatioCodec          eiAspectRatio

-- | Codec for 'EmbedImages'.
embedImagesCodec :: Codec EmbedImages
embedImagesCodec =
    Codec.record "app.bsky.embed.images" $
        EmbedImages
            <$> Codec.requiredField "images" (Codec.array embedImageCodec) eisImages

-- | Codec for 'VideoCaption'.
videoCaptionCodec :: Codec VideoCaption
videoCaptionCodec =
    Codec.record "app.bsky.embed.video#caption" $
        VideoCaption
            <$> Codec.requiredField "lang" (Codec.string LexFormatLanguage) vcLang
            <*> Codec.requiredField "file" Codec.blob                       vcFile

-- | Codec for 'EmbedVideo'.
embedVideoCodec :: Codec EmbedVideo
embedVideoCodec =
    Codec.record "app.bsky.embed.video" $
        EmbedVideo
            <$> Codec.requiredField "video"       Codec.blob                          evVideo
            <*> Codec.optionalField "captions"    (Codec.array videoCaptionCodec)     evCaptions
            <*> Codec.optionalField "alt"         Codec.text                          evAlt
            <*> Codec.optionalField "aspectRatio" aspectRatioCodec                    evAspectRatio

-- | Codec for 'ExternalEmbed'.
externalEmbedCodec :: Codec ExternalEmbed
externalEmbedCodec =
    Codec.record "app.bsky.embed.external#external" $
        ExternalEmbed
            <$> Codec.requiredField "uri"         Codec.uri        eeUri
            <*> Codec.requiredField "title"       Codec.text       eeTitle
            <*> Codec.requiredField "description" Codec.text       eeDescription
            <*> Codec.optionalField "thumb"       Codec.blob       eeThumb

-- | Codec for 'EmbedExternal'.
embedExternalCodec :: Codec EmbedExternal
embedExternalCodec =
    Codec.record "app.bsky.embed.external" $
        EmbedExternal
            <$> Codec.requiredField "external" externalEmbedCodec eextExternal

-- | Codec for 'EmbedRecord'.
embedRecordCodec :: Codec EmbedRecord
embedRecordCodec =
    Codec.record "app.bsky.embed.record" $
        EmbedRecord
            <$> Codec.requiredField "record" strongRefCodec erecRecord

-- | Codec for the 'MediaEmbed' union.
mediaEmbedCodec :: Codec MediaEmbed
mediaEmbedCodec =
  let
    from = \case
      MediaEmbedImages a -> Left a
      MediaEmbedVideo a -> Right (Left a)
      MediaEmbedExternal a -> Right (Right a)

    to = \case
      Left a -> MediaEmbedImages a
      Right (Left a) -> MediaEmbedVideo a
      Right (Right a) -> MediaEmbedExternal a
  in
  Codec.invmap to from $
    Codec.union3
      embedImagesCodec
      embedVideoCodec
      embedExternalCodec

-- | Codec for 'EmbedRecordWithMedia'.
embedRecordWithMediaCodec :: Codec EmbedRecordWithMedia
embedRecordWithMediaCodec =
    Codec.record "app.bsky.embed.recordWithMedia" $
        EmbedRecordWithMedia
            <$> Codec.requiredField "record" embedRecordCodec erwmRecord
            <*> Codec.requiredField "media"  mediaEmbedCodec  erwmMedia

-- | Codec for the 'PostEmbed' union.
postEmbedCodec :: Codec PostEmbed
postEmbedCodec =
  let
    from = \case
      PostEmbedImages a -> Left (Left a)
      PostEmbedVideo a -> Left (Right a)
      PostEmbedExternal a -> Right (Left a)
      PostEmbedRecord a -> Right (Right (Left a))
      PostEmbedRecordWithMedia a -> Right (Right (Right a))

    to = \case
      Left (Left a) -> PostEmbedImages a
      Left (Right a) -> PostEmbedVideo a
      Right (Left a) -> PostEmbedExternal a
      Right (Right (Left a)) -> PostEmbedRecord a
      Right (Right (Right a)) -> PostEmbedRecordWithMedia a
  in
  Codec.invmap to from $
    Codec.union5
      embedImagesCodec
      embedVideoCodec
      embedExternalCodec
      embedRecordCodec
      embedRecordWithMediaCodec


-- | Codec for 'SelfLabelValue'.
selfLabelValueCodec :: Codec SelfLabelValue
selfLabelValueCodec =
    Codec.record "com.atproto.label.defs#selfLabelValue" $
        SelfLabelValue
            <$> Codec.requiredField "val" Codec.text slvVal

-- | Codec for 'SelfLabels'.
selfLabelsCodec :: Codec SelfLabels
selfLabelsCodec =
    Codec.record "com.atproto.label.defs#selfLabels" $
        SelfLabels
            <$> Codec.requiredField "values" (Codec.array selfLabelValueCodec) slValues

-- | Codec for the 'PostLabels' union.
postLabelsCodec :: Codec PostLabels
postLabelsCodec =
  let
    from (PostLabelsSelf a) = a
    to = PostLabelsSelf
  in
  Codec.invmap to from $
    selfLabelsCodec


-- | Codec for 'FeedPost'.
--
-- Uses 'Codec.namedType' to prevent infinite schema expansion.
feedPostCodec :: Codec FeedPost
feedPostCodec =
    Codec.namedType "app.bsky.feed.post" $
    Codec.record "app.bsky.feed.post" $
        FeedPost
            <$> Codec.requiredField "text"      Codec.text                              fpText
            <*> Codec.requiredField "createdAt" Codec.datetime                          fpCreatedAt
            <*> Codec.optionalField "reply"     replyRefCodec                           fpReply
            <*> Codec.optionalField "facets"    (Codec.array facetCodec)                fpFacets
            <*> Codec.optionalField "embed"     postEmbedCodec                          fpEmbed
            <*> Codec.optionalField "langs"     (Codec.array (Codec.string LexFormatLanguage)) fpLangs
            <*> Codec.optionalField "labels"    postLabelsCodec                         fpLabels
            <*> Codec.optionalField "tags"      (Codec.array Codec.text)                fpTags
