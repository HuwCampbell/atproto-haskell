{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
-- | Core codec combinators for the AT Protocol Lexicon type system.
--
-- A 'Codec' @a@ bundles a 'LexSchema' (for documentation), a decoder from
-- 'LexValue', and an encoder to 'LexValue'.  Because the three components
-- always travel together they can never drift apart.
--
-- Codecs compose via 'invmap' (iso-map) and the 'StructBuilder' applicative.
-- The 'Profunctor' instance on 'StructBuilder' lets you use @lmap@ to
-- project fields out of a record when encoding.
module ATProto.Lex.Codec
  ( -- * Errors
    LexError (..)
    -- * Codec
  , Codec (..)
    -- ** Mapping
  , invmap
  , emap
  , namedType
    -- * Primitives
  , null
  , bool
  , int
  , text
  , string
  , bytes
  , cid
  , blob
  , datetime
  , atUri
  , did
  , handle
  , uri
  , lexValue
    -- * Composites
  , array
  , nullable
  , fallback
    -- * Object / record builder
  , StructBuilder
  , StructCodec
  , record
  , requiredField
  , optionalField
  , fallbackField
    -- * Union
  , union
  , union3
  , union4
  , union5
  ) where

import           Prelude             hiding (null)

import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import           Data.ByteString     (ByteString)
import           Data.Int            (Int64)
import           Data.Profunctor     (Profunctor (..))

import           ATProto.Ipld.Value  (BlobRef (..), Cid, LexValue (..))
import           ATProto.Lex.Schema

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors that can occur while decoding a 'LexValue'.
data LexError
  = MissingField   T.Text
    -- ^ A required object field was absent.
  | TypeMismatch   T.Text LexValue
    -- ^ A value did not match the expected type.
  | InvalidValue   T.Text T.Text
    -- ^ A value was syntactically well-typed but semantically invalid.
  | UnknownVariant T.Text
    -- ^ A @$type@ tag did not match any registered variant.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Codec
-- ---------------------------------------------------------------------------

-- | A codec for values of type @a@.
--
-- Carries a 'LexSchema' (for documentation\/introspection), a decoder from
-- 'LexValue', and an encoder (writer) to 'LexValue'.
data Codec a = Codec
  { schema  :: LexSchema
    -- ^ The structural schema of the type.
  , decoder :: LexValue -> Either LexError a
    -- ^ Decode a 'LexValue' into @a@, or return a 'LexError'.
  , writer  :: a -> LexValue
    -- ^ Encode an @a@ into a 'LexValue'.
  }

-- ---------------------------------------------------------------------------
-- Mapping
-- ---------------------------------------------------------------------------

-- | Map a codec isomorphically.
invmap :: (a -> b) -> (b -> a) -> Codec a -> Codec b
invmap f g c = Codec
  { schema  = schema c
  , decoder = fmap f . decoder c
  , writer  = writer c . g
  }

-- | Map the decoder with a fallible function, keeping the encoder side.
emap :: (a -> Either LexError b) -> (b -> a) -> Codec a -> Codec b
emap f g c = Codec
  { schema  = schema c
  , decoder = decoder c >=> f
  , writer  = writer c . g
  }

-- | Collapse the schema to a 'LexSchemaRef' to prevent infinite expansion
--   for recursive or named types.
namedType :: T.Text -> Codec a -> Codec a
namedType name c = c { schema = LexSchemaRef name }

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

-- | Codec for the unit value.
null :: Codec ()
null = Codec
  { schema  = LexSchemaNull
  , decoder = \v -> case v of
        LexNull -> Right ()
        _       -> Left (TypeMismatch "null" v)
  , writer  = const LexNull
  }

-- | Codec for booleans.
bool :: Codec Bool
bool = Codec
  { schema  = LexSchemaBool
  , decoder = \v -> case v of
        LexBool b -> Right b
        _         -> Left (TypeMismatch "bool" v)
  , writer  = LexBool
  }

-- | Codec for 64-bit signed integers.
int :: Codec Int64
int = Codec
  { schema  = LexSchemaInt
  , decoder = \v -> case v of
        LexInt n -> Right n
        _        -> Left (TypeMismatch "int" v)
  , writer  = LexInt
  }

-- | Codec for plain UTF-8 strings (no format constraint).
text :: Codec T.Text
text = Codec
  { schema  = LexSchemaString Nothing
  , decoder = \v -> case v of
        LexString t -> Right t
        _           -> Left (TypeMismatch "string" v)
  , writer  = LexString
  }

-- | Codec for a string with a specific format constraint.
string :: LexFormat -> Codec T.Text
string fmt = Codec
  { schema  = LexSchemaString (Just fmt)
  , decoder = \v -> case v of
        LexString t -> Right t
        _           -> Left (TypeMismatch "string" v)
  , writer  = LexString
  }

-- | Codec for raw byte arrays.
bytes :: Codec ByteString
bytes = Codec
  { schema  = LexSchemaBytes
  , decoder = \v -> case v of
        LexBytes bs -> Right bs
        _           -> Left (TypeMismatch "bytes" v)
  , writer  = LexBytes
  }

-- | Codec for CID links.
cid :: Codec Cid
cid = Codec
  { schema  = LexSchemaCid
  , decoder = \v -> case v of
        LexLink c -> Right c
        _         -> Left (TypeMismatch "cid-link" v)
  , writer  = LexLink
  }

-- | Codec for blob references.
--
-- Encodes and decodes 'BlobRef' values, which appear in the AT Protocol
-- data model as @{\"$type\":\"blob\", \"ref\":{...}, \"mimeType\":\"...\", \"size\":...}@
-- objects.
blob :: Codec BlobRef
blob = Codec
  { schema  = LexSchemaBlob
  , decoder = \v -> case v of
        LexBlob b -> Right b
        _         -> Left (TypeMismatch "blob" v)
  , writer  = LexBlob
  }

-- | Codec for @datetime@-format strings.
datetime :: Codec T.Text
datetime = string LexFormatDatetime

-- | Codec for @at-uri@-format strings.
atUri :: Codec T.Text
atUri = string LexFormatAtUri

-- | Codec for @did@-format strings.
did :: Codec T.Text
did = string LexFormatDid

-- | Codec for @handle@-format strings.
handle :: Codec T.Text
handle = string LexFormatHandle

-- | Codec for generic @uri@-format strings.
uri :: Codec T.Text
uri = string LexFormatUri

-- | Passthrough codec for raw 'LexValue' fields.
--
-- Use this for record fields whose schema is determined by the NSID collection
-- at runtime rather than by the binding layer.
lexValue :: Codec LexValue
lexValue = Codec
  { schema  = LexSchemaUnknown
  , decoder = Right
  , writer  = id
  }

-- ---------------------------------------------------------------------------
-- Composites
-- ---------------------------------------------------------------------------

-- | Codec for a homogeneous list.
array :: Codec a -> Codec [a]
array c = Codec
  { schema  = LexSchemaArray (schema c)
  , decoder = \v -> case v of
        LexArray vs -> mapM (decoder c) vs
        _           -> Left (TypeMismatch "array" v)
  , writer  = LexArray . map (writer c)
  }

-- | Lift a codec to handle optional ('Maybe') values.
--
-- 'Nothing' encodes to 'LexNull'; 'LexNull' (and absent fields) decodes to
-- 'Nothing'.
nullable :: Codec a -> Codec (Maybe a)
nullable c = Codec
  { schema  = LexSchemaMaybe (schema c)
  , decoder = \v -> case v of
        LexNull -> Right Nothing
        _       -> fmap Just (decoder c v)
  , writer  = maybe LexNull (writer c)
  }

-- | Wrap a codec with a default value used when decoding fails.
fallback :: a -> Codec a -> Codec a
fallback def c = Codec
  { schema  = schema c
  , decoder = \v -> case decoder c v of
        Left  _ -> Right def
        Right a -> Right a
  , writer  = writer c
  }

-- ---------------------------------------------------------------------------
-- Object / record builder
-- ---------------------------------------------------------------------------

-- | Difference list
--   This is used for performance reasons in the Struct Builder, so that
--   concatenation and building isn't O(N²).
type DList a = [a] -> [a]

-- | A builder for object codecs.
--
-- @StructBuilder b a@ is:
--
--   * __Covariant__ in @a@: the decoded result type.
--   * __Contravariant__ in @b@: the source record type used during encoding.
--
-- Build a complete struct codec with 'record', combining field builders with
-- 'requiredField', 'optionalField', and 'fallbackField' using '<*>'.
data StructBuilder b a = StructBuilder
  { sbFields  :: DList LexField
  , sbDecoder :: Map.Map T.Text LexValue -> Either LexError a
  , sbWriter  :: b -> DList (T.Text, LexValue)
  }

instance Functor (StructBuilder b) where
  fmap f sb = sb { sbDecoder = fmap f . sbDecoder sb }

instance Applicative (StructBuilder b) where
  pure x = StructBuilder
    { sbFields  = id
    , sbDecoder = const (Right x)
    , sbWriter  = const id
    }
  sf <*> sx = StructBuilder
    { sbFields  = sbFields sf . sbFields sx
    , sbDecoder = \m -> sbDecoder sf m <*> sbDecoder sx m
    , sbWriter  = \b -> sbWriter sf b . sbWriter sx b
    }

instance Profunctor StructBuilder where
  dimap f g sb = StructBuilder
    { sbFields  = sbFields sb
    , sbDecoder = fmap g . sbDecoder sb
    , sbWriter  = sbWriter sb . f
    }

-- | A 'StructBuilder' where the source and result type are the same.
type StructCodec a = StructBuilder a a

-- | Construct a 'Codec' for a record type from a 'StructCodec'.
--
-- The @name@ is used as the @$type@ discriminator embedded in every
-- encoded object.
record :: T.Text -> StructCodec a -> Codec a
record name sb = Codec
  { schema  = LexSchemaObject name (sbFields sb [])
  , decoder = \v -> case v of
        LexObject m -> sbDecoder sb m
        _           -> Left (TypeMismatch name v)
  , writer  = \a ->
        LexObject (Map.fromList (sbWriter sb a []))
  }

-- | Build a required field.
--
-- Decoding fails with 'MissingField' if the key is absent.
requiredField
  :: T.Text          -- ^ Field name (JSON key)
  -> Codec a         -- ^ Value codec
  -> (b -> a)        -- ^ Projection from the source record
  -> StructBuilder b a
requiredField name c proj = StructBuilder
  { sbFields  = \more -> LexField name (schema c) True Nothing : more
  , sbDecoder = \m ->
        case Map.lookup name m of
            Nothing -> Left (MissingField name)
            Just v  -> decoder c v
  , sbWriter  = \b more -> (name, writer c (proj b)) : more
  }

-- | Build an optional field.
--
-- Decoding yields 'Nothing' when the key is absent or the value is 'LexNull'.
optionalField
  :: T.Text          -- ^ Field name (JSON key)
  -> Codec a         -- ^ Value codec
  -> (b -> Maybe a)  -- ^ Projection from the source record
  -> StructBuilder b (Maybe a)
optionalField name c proj = StructBuilder
  { sbFields  = \more -> LexField name (LexSchemaMaybe (schema c)) False Nothing : more
  , sbDecoder = \m ->
        case Map.lookup name m of
            Nothing      -> Right Nothing
            Just LexNull -> Right Nothing
            Just v       -> fmap Just (decoder c v)
  , sbWriter  = \b ->
        case proj b of
            Nothing -> id
            Just a  -> \more -> (name, writer c a) : more
  }

-- | Build a field with a default value used when the key is absent.
fallbackField
  :: T.Text          -- ^ Field name (JSON key)
  -> Codec a         -- ^ Value codec
  -> a               -- ^ Default value
  -> (b -> a)        -- ^ Projection from the source record
  -> StructBuilder b a
fallbackField name c def proj = StructBuilder
  { sbFields  = \more -> LexField name (schema c) False Nothing : more
  , sbDecoder = \m ->
        case Map.lookup name m of
            Nothing -> Right def
            Just v  -> decoder c v
  , sbWriter  = \b more -> (name, writer c (proj b)) : more
  }

-- ---------------------------------------------------------------------------
-- Union
-- ---------------------------------------------------------------------------

-- | Build a union codec from a list of variants.
--
-- Each variant contributes a @$type@ tag; the first variant whose tag matches
-- the @$type@ field in the object is used for decoding.
--
-- During encoding, each variant's writer is tried in order; the first one
-- that produces a non-'Nothing' result is used.
union :: Codec a -> Codec b -> Codec (Either a b)
union leftCodec rightCodec =
  let
    leftInject =
      injectVariant leftCodec
    rightInject =
      injectVariant rightCodec
    leftVariants =
      uvSchemas leftInject
    rightVariants =
      uvSchemas rightInject
  in
    Codec {
      schema = LexSchemaUnion (leftVariants <> rightVariants)
    , decoder = \v -> case v of
        LexObject m ->
          case Map.lookup "$type" m of
            Nothing -> Left (MissingField "$type")
            Just (LexString tag) ->
              if tag `elem` (variantType <$> leftVariants) then
                Left <$> uvDecoder leftInject v
              else
                Right <$> uvDecoder rightInject v
            Just bad -> Left (TypeMismatch "$type" bad)
        _ -> Left (TypeMismatch "union" v)
    , writer = either (uvWriter leftInject) (uvWriter rightInject)
    }
{-# INLINABLE union #-}

{-| Construct a union from 3 codecs.
-}
union3 :: Codec a -> Codec b -> Codec c -> Codec (Either a (Either b c))
union3 a b c =
     a `union` union b c
{-# INLINABLE union3 #-}


{-| Construct a union from 4 codecs.
-}
union4 :: Codec a -> Codec b -> Codec c -> Codec d -> Codec (Either (Either a b) (Either c d))
union4 a b c d =
    union a b `union` union c d
{-# INLINABLE union4 #-}


{-| Construct a union from 5 codecs.
-}
union5 :: Codec a -> Codec b -> Codec c -> Codec d -> Codec e -> Codec (Either (Either a b) (Either c (Either d e)))
union5 a b c d e =
    union a b `union` union3 c d e
{-# INLINABLE union5 #-}

{- | Intermediate type used after we inject -}
data LexUnionVariantCodec a =
  LexUnionVariantCodec {
    uvSchemas :: [LexUnionVariant]
  , uvDecoder :: LexValue -> Either LexError a
  , uvWriter  :: a -> LexValue
  }

{-| Create a Union Variant from a Codec.
    For unions, this is a no-op, otherwise
    we just use the names of the record or ref.
-}
injectVariant
  :: Codec a
  -> LexUnionVariantCodec a
injectVariant bodyCodec =
  case schema bodyCodec of
    LexSchemaUnion schemas ->
      LexUnionVariantCodec
        schemas
        (decoder bodyCodec)
        (writer bodyCodec)
    LexSchemaObject name _ ->
      unionVariant name bodyCodec
    LexSchemaRef name ->
      unionVariant name bodyCodec
    _ ->
      LexUnionVariantCodec {
        uvSchemas  =  []
      , uvDecoder = const $ Left (TypeMismatch "union" LexNull)
      , uvWriter  = const LexNull
      }
{-# INLINE injectVariant #-}

-- | Build a single variant for use with 'union'.
--
-- @unionVariant tag bodyCodec toMaybe fromVariant@ produces a 'Codec' @s@
-- where:
--
--   * Encoding: if @toMaybe s@ returns 'Just a', encode @a@ with @bodyCodec@
--     and inject @$type = tag@ into the resulting object; otherwise encode
--     the value as 'LexNull' (this branch should not be selected by 'union').
--   * Decoding: look for @$type == tag@ and decode the body with @bodyCodec@,
--     then lift into @s@ via @fromVariant@.
unionVariant
  :: T.Text          -- ^ @$type@ tag
  -> Codec a         -- ^ Body codec
  -> LexUnionVariantCodec a
unionVariant tag bodyCodec = LexUnionVariantCodec
  { uvSchemas = [LexUnionVariant tag (schema bodyCodec)]
  , uvDecoder = \v -> case v of
      LexObject m ->
        case Map.lookup "$type" m of
          Just (LexString t) | t == tag ->
            decoder bodyCodec v
          Just (LexString t) ->
            Left (UnknownVariant t)
          _ ->
            Left (MissingField "$type")
      _ -> Left (TypeMismatch tag v)
  , uvWriter = \s ->
      case writer bodyCodec s of
        LexObject m ->
          LexObject (Map.insert "$type" (LexString tag) m)
        other ->
          LexObject (Map.fromList
            [ ("$type", LexString tag)
            , ("value", other)
            ])
  }
{-# INLINE unionVariant #-}

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

(>=>) :: (a -> Either e b) -> (b -> Either e c) -> a -> Either e c
f >=> g = \a -> f a >>= g
