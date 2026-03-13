-- | Structural schema types embedded in codecs.
--
-- A 'LexSchema' describes the shape of a value in the AT Protocol data model.
-- It is carried inside every 'ATProto.Lex.Codec.Codec' so that the schema,
-- encoder, and decoder are always kept in sync.
module ATProto.Lex.Schema
  ( -- * Schema
    LexSchema (..)
    -- * Fields and variants
  , LexField (..)
  , LexUnionVariant (..)
    -- * String formats
  , LexFormat (..)
    -- * Rendering
  , renderSchema
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text  as T

-- | String format constraints for 'LexSchemaString'.
data LexFormat
  = LexFormatDatetime  -- ^ RFC 3339 datetime
  | LexFormatAtUri     -- ^ AT-URI
  | LexFormatDid       -- ^ DID
  | LexFormatHandle    -- ^ Handle
  | LexFormatNsid      -- ^ NSID
  | LexFormatCid       -- ^ CID string
  | LexFormatUri       -- ^ Generic URI
  | LexFormatLanguage  -- ^ BCP-47 language tag
  | LexFormatTid       -- ^ TID (timestamp ID)
  | LexFormatRecordKey -- ^ Record key
  deriving (Eq, Show)

-- | A single field inside an object schema.
data LexField = LexField
  { fieldName     :: T.Text
    -- ^ The JSON key for this field.
  , fieldSchema   :: LexSchema
    -- ^ The schema of the field's value.
  , fieldRequired :: Bool
    -- ^ Whether the field must be present.
  , fieldDoc      :: Maybe T.Text
    -- ^ Optional human-readable description.
  } deriving (Eq, Show)

-- | One alternative in a union schema.
data LexUnionVariant = LexUnionVariant
  { variantType   :: T.Text
    -- ^ The @$type@ tag (NSID or NSID#fragment).
  , variantSchema :: LexSchema
    -- ^ The schema for the body when this variant is active.
  } deriving (Eq, Show)

-- | The structural schema of an AT Protocol value.
--
-- This mirrors the Lexicon schema language and is used purely for
-- documentation\/introspection; the actual encode\/decode logic lives in
-- 'ATProto.Lex.Codec.Codec'.
data LexSchema
  = LexSchemaNull
    -- ^ The unit\/null type.
  | LexSchemaBool
    -- ^ A boolean.
  | LexSchemaInt
    -- ^ A 64-bit signed integer.
  | LexSchemaString  (Maybe LexFormat)
    -- ^ A UTF-8 string, optionally constrained to a format.
  | LexSchemaBytes
    -- ^ A raw byte array.
  | LexSchemaCid
    -- ^ A CID link.
  | LexSchemaArray   LexSchema
    -- ^ An ordered sequence of values sharing a common schema.
  | LexSchemaObject  [LexField]
    -- ^ A string-keyed map with named, typed fields.
  | LexSchemaUnion   [LexUnionVariant]
    -- ^ A @$type@-discriminated sum type.
  | LexSchemaMaybe   LexSchema
    -- ^ An optional value (the field may be absent or null).
  | LexSchemaRef     T.Text
    -- ^ A named reference (NSID or @#fragment@).  Used to prevent infinite
    --   schema expansion for recursive types.
  | LexSchemaUnknown
    -- ^ A passthrough schema for raw 'ATProto.Ipld.Value.LexValue' fields
    --   whose structure is determined at runtime by the NSID collection.
  deriving (Eq, Show)

-- | Render a 'LexSchema' as a JSON @Value@ matching the Lexicon spec fragment.
renderSchema :: LexSchema -> Aeson.Value
renderSchema LexSchemaNull       = Aeson.object ["type" Aeson..= ("null"    :: T.Text)]
renderSchema LexSchemaBool       = Aeson.object ["type" Aeson..= ("boolean" :: T.Text)]
renderSchema LexSchemaInt        = Aeson.object ["type" Aeson..= ("integer" :: T.Text)]
renderSchema (LexSchemaString f) =
    Aeson.object $ ["type" Aeson..= ("string" :: T.Text)]
                ++ maybe [] (\fmt -> ["format" Aeson..= renderFormat fmt]) f
renderSchema LexSchemaBytes      = Aeson.object ["type" Aeson..= ("bytes"   :: T.Text)]
renderSchema LexSchemaCid        = Aeson.object ["type" Aeson..= ("cid-link" :: T.Text)]
renderSchema (LexSchemaArray s)  =
    Aeson.object
        [ "type"  Aeson..= ("array" :: T.Text)
        , "items" Aeson..= renderSchema s
        ]
renderSchema (LexSchemaObject fs) =
    Aeson.object
        [ "type"       Aeson..= ("object" :: T.Text)
        , "required"   Aeson..= [fieldName f | f <- fs, fieldRequired f]
        , "properties" Aeson..= Aeson.object
            [ (fieldName f Aeson..= renderSchema (fieldSchema f))
            | f <- fs
            ]
        ]
renderSchema (LexSchemaUnion vs) =
    Aeson.object
        [ "type" Aeson..= ("union" :: T.Text)
        , "refs" Aeson..= map variantType vs
        ]
renderSchema (LexSchemaMaybe s)  = renderSchema s
renderSchema (LexSchemaRef r)    = Aeson.object ["type" Aeson..= ("ref" :: T.Text), "ref" Aeson..= r]
renderSchema LexSchemaUnknown    = Aeson.object ["type" Aeson..= ("unknown" :: T.Text)]

renderFormat :: LexFormat -> T.Text
renderFormat LexFormatDatetime  = "datetime"
renderFormat LexFormatAtUri     = "at-uri"
renderFormat LexFormatDid       = "did"
renderFormat LexFormatHandle    = "handle"
renderFormat LexFormatNsid      = "nsid"
renderFormat LexFormatCid       = "cid"
renderFormat LexFormatUri       = "uri"
renderFormat LexFormatLanguage  = "language"
renderFormat LexFormatTid       = "tid"
renderFormat LexFormatRecordKey = "record-key"
