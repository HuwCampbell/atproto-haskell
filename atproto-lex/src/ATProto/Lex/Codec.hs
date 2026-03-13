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
  , unionVariant
  ) where

import           Prelude             hiding (null)

import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import           Data.ByteString     (ByteString)
import           Data.Int            (Int64)
import           Data.Profunctor     (Profunctor (..))

import ATProto.Ipld.Value (Cid, LexValue (..))
import ATProto.Lex.Schema

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
  { sbFields  :: [LexField]
  , sbDecoder :: Map.Map T.Text LexValue -> Either LexError a
  , sbWriter  :: b -> [(T.Text, LexValue)]
  }

instance Functor (StructBuilder b) where
  fmap f sb = sb { sbDecoder = fmap f . sbDecoder sb }

instance Applicative (StructBuilder b) where
  pure x = StructBuilder
    { sbFields  = []
    , sbDecoder = const (Right x)
    , sbWriter  = const []
    }
  sf <*> sx = StructBuilder
    { sbFields  = sbFields sf ++ sbFields sx
    , sbDecoder = \m -> sbDecoder sf m <*> sbDecoder sx m
    , sbWriter  = \b -> sbWriter sf b ++ sbWriter sx b
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
  { schema  = LexSchemaObject (sbFields sb)
  , decoder = \v -> case v of
        LexObject m -> sbDecoder sb m
        _           -> Left (TypeMismatch name v)
  , writer  = \a ->
        LexObject (Map.fromList (sbWriter sb a))
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
  { sbFields  = [LexField name (schema c) True Nothing]
  , sbDecoder = \m ->
        case Map.lookup name m of
            Nothing -> Left (MissingField name)
            Just v  -> decoder c v
  , sbWriter  = \b -> [(name, writer c (proj b))]
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
  { sbFields  = [LexField name (LexSchemaMaybe (schema c)) False Nothing]
  , sbDecoder = \m ->
        case Map.lookup name m of
            Nothing      -> Right Nothing
            Just LexNull -> Right Nothing
            Just v       -> fmap Just (decoder c v)
  , sbWriter  = \b ->
        case proj b of
            Nothing -> []
            Just a  -> [(name, writer c a)]
  }

-- | Build a field with a default value used when the key is absent.
fallbackField
  :: T.Text          -- ^ Field name (JSON key)
  -> Codec a         -- ^ Value codec
  -> a               -- ^ Default value
  -> (b -> a)        -- ^ Projection from the source record
  -> StructBuilder b a
fallbackField name c def proj = StructBuilder
  { sbFields  = [LexField name (schema c) False Nothing]
  , sbDecoder = \m ->
        case Map.lookup name m of
            Nothing -> Right def
            Just v  -> decoder c v
  , sbWriter  = \b -> [(name, writer c (proj b))]
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
union :: [Codec a] -> Codec a
union variants = Codec
  { schema  = LexSchemaUnion []
  , decoder = \v -> case v of
        LexObject m ->
            case Map.lookup "$type" m of
                Nothing -> Left (MissingField "$type")
                Just (LexString tag) ->
                    let go [] = Left (UnknownVariant tag)
                        go (c:cs) = case decoder c v of
                            Right a -> Right a
                            Left  _ -> go cs
                    in  go variants
                Just bad -> Left (TypeMismatch "$type" bad)
        _ -> Left (TypeMismatch "union" v)
  , writer  = \a ->
        -- Try each variant's writer in order; variants that don't handle
        -- the given constructor return LexNull as a sentinel value, which
        -- is safe because union-typed values always encode to LexObject.
        let go []     = LexNull
            go (c:cs) = case writer c a of
                LexNull -> go cs
                result  -> result
        in  go variants
  }

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
  -> (s -> Maybe a)  -- ^ Project @a@ out of the sum type for encoding
  -> (a -> s)        -- ^ Inject @a@ into the sum type after decoding
  -> Codec s
unionVariant tag bodyCodec toMaybe fromVariant = Codec
  { schema  = LexSchemaUnion [LexUnionVariant tag (schema bodyCodec)]
  , decoder = \v -> case v of
        LexObject m ->
            case Map.lookup "$type" m of
                Just (LexString t) | t == tag ->
                    fmap fromVariant (decoder bodyCodec v)
                Just (LexString t) ->
                    Left (UnknownVariant t)
                _ ->
                    Left (MissingField "$type")
        _ -> Left (TypeMismatch tag v)
  , writer  = \s ->
        case toMaybe s of
            Nothing -> LexNull
            Just a  ->
                case writer bodyCodec a of
                    LexObject m ->
                        LexObject (Map.insert "$type" (LexString tag) m)
                    other ->
                        LexObject (Map.fromList
                            [ ("$type", LexString tag)
                            , ("value", other)
                            ])
  }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

(>=>) :: (a -> Either e b) -> (b -> Either e c) -> a -> Either e c
f >=> g = \a -> f a >>= g
