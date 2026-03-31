-- | Validate that a 'Codec'\'s embedded 'LexSchema' agrees with a parsed
-- 'LexiconDoc'.
--
-- When writing typed bindings for AT Protocol methods (e.g. in
-- @atproto-haskell-repo@), each binding manually constructs a 'Codec' with
-- field names, types, and requiredness.  It is easy for these to drift from
-- the upstream lexicon JSON.  This module provides a small structural
-- validator that compares the 'LexSchema' carried inside a 'Codec' against
-- the corresponding definition in a parsed 'LexiconDoc' and reports any
-- mismatches.
--
-- = Usage
--
-- @
-- import qualified Data.Aeson as Aeson
-- import ATProto.Lex.Validate
-- import ATProto.Lexicon
-- import ATProto.Repo.ListRecords (listRecordsResponseCodec)
-- import ATProto.Lex.Codec (schema)
--
-- -- Parse the lexicon JSON (from disk or inline)
-- Right doc = Aeson.eitherDecode lexiconBytes :: Either String LexiconDoc
--
-- -- Validate the response codec against the \"record\" def
-- validate doc "record" (schema listRecordsResponseCodec)
-- -- [] means no errors
-- @
module ATProto.Lex.Validate
  ( -- * Validation
    validate
    -- * Error type
  , ValidationError (..)
    -- * Path
  , Path
  ) where

import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Lex.Schema      (LexSchema (..), LexField (..), LexFormat (..), LexUnionVariant (..))
import           ATProto.Lexicon.Types

-- | A dotted path into the schema tree, for error localisation.
--
-- Example: @[\"output\", \"records\", \"items\"]@.
type Path = [T.Text]

-- | A mismatch between a codec's schema and a lexicon definition.
data ValidationError
  = FieldMissing
      { vePath   :: Path
        -- ^ Where the field was expected.
      , veField  :: T.Text
        -- ^ The field name the codec declares but the lexicon does not.
      }
  | FieldExtra
      { vePath   :: Path
        -- ^ Where the extra field was found.
      , veField  :: T.Text
        -- ^ The field name the lexicon declares but the codec does not.
      }
  | TypeMismatch
      { vePath     :: Path
        -- ^ Where the mismatch occurred.
      , veExpected :: T.Text
        -- ^ A short description of the lexicon type.
      , veActual   :: T.Text
        -- ^ A short description of the codec schema type.
      }
  | RequiredMismatch
      { vePath     :: Path
        -- ^ Where the mismatch occurred.
      , veField    :: T.Text
        -- ^ The field name.
      , veExpectedRequired :: Bool
        -- ^ Whether the lexicon says the field is required.
      , veActualRequired   :: Bool
        -- ^ Whether the codec says the field is required.
      }
  | DefinitionNotFound
      { vePath     :: Path
        -- ^ Where the lookup was attempted.
      , veDefName  :: T.Text
        -- ^ The definition name that was not found.
      }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Validate a codec's 'LexSchema' against a named definition in a
-- 'LexiconDoc'.
--
-- The @defName@ is a key into @lexDocDefs doc@ — typically @\"main\"@ for the
-- primary entry point, or a @#fragment@ name like @\"record\"@.
--
-- Returns @[]@ when the schema matches the lexicon perfectly (modulo
-- information that codecs don't track, such as description strings and
-- value constraints like min\/max).
validate :: LexiconDoc -> T.Text -> LexSchema -> [ValidationError]
validate doc defName codecSchema =
  case Map.lookup defName (lexDocDefs doc) of
    Nothing ->
      [DefinitionNotFound [] defName]
    Just userType ->
      validateUserType [] doc userType codecSchema

-- ---------------------------------------------------------------------------
-- Internal: top-level user type dispatch
-- ---------------------------------------------------------------------------

validateUserType :: Path -> LexiconDoc -> LexUserType -> LexSchema -> [ValidationError]
validateUserType path doc (LexObject' obj) s =
    validateObject path doc obj s
validateUserType path doc (LexRecord' rec) s =
    validateObject path doc (lexRecordRecord rec) s
validateUserType path doc (LexQuery q) s =
    case lexXrpcQueryOutput q of
      Just body -> validateBody path doc body s
      Nothing   -> validateBody path doc (LexXrpcBody Nothing "application/json" Nothing) s
validateUserType path doc (LexProcedure p) s =
    case lexXrpcProcOutput p of
      Just body -> validateBody path doc body s
      Nothing   -> validateBody path doc (LexXrpcBody Nothing "application/json" Nothing) s
validateUserType path _doc (LexString' lexStr) s =
    validatePrimitive path (LexPrimString lexStr) s
validateUserType path _doc (LexToken' _) s =
    -- Tokens are simple named constants; any schema is acceptable
    case s of
      LexSchemaObject _ -> []
      _                 -> [TypeMismatch path "token" (describeSchema s)]
validateUserType path _doc (LexBlob' _) s =
    case s of
      LexSchemaBlob -> []
      _             -> [TypeMismatch path "blob" (describeSchema s)]
validateUserType path _doc (LexBytes' _) s =
    case s of
      LexSchemaBytes -> []
      _              -> [TypeMismatch path "bytes" (describeSchema s)]
validateUserType path _doc (LexCidLink' _) s =
    case s of
      LexSchemaCid -> []
      _            -> [TypeMismatch path "cid-link" (describeSchema s)]
validateUserType path doc (LexArray' arr) s =
    validateArray path doc arr s
validateUserType path doc (LexSubscription sub) s =
    case lexXrpcSubMessage sub of
      Just body -> validateBody path doc body s
      Nothing   -> []

-- ---------------------------------------------------------------------------
-- Body (XRPC input/output)
-- ---------------------------------------------------------------------------

validateBody :: Path -> LexiconDoc -> LexXrpcBody -> LexSchema -> [ValidationError]
validateBody path doc body s =
    case lexXrpcBodySchema body of
      Nothing -> []
      Just (Left obj) ->
          validateObject path doc obj s
      Just (Right refVariant) ->
          validateRefVariant path doc refVariant s

-- ---------------------------------------------------------------------------
-- Object
-- ---------------------------------------------------------------------------

validateObject :: Path -> LexiconDoc -> LexObject -> LexSchema -> [ValidationError]
validateObject path doc obj codecSchema =
    case unwrapObject codecSchema of
      Just fields -> checkObject path doc obj fields
      Nothing     -> [TypeMismatch path "object" (describeSchema codecSchema)]
  where
    unwrapObject (LexSchemaObject fs) = Just fs
    unwrapObject _                    = Nothing

checkObject :: Path -> LexiconDoc -> LexObject -> [LexField] -> [ValidationError]
checkObject path doc obj codecFields =
    let requiredSet    = maybe [] id (lexObjRequired obj)
        lexProps       = lexObjProperties obj
        codecFieldMap  = Map.fromList [(fieldName f, f) | f <- codecFields]
        -- Fields in codec but not in lexicon
        extraInCodec   = [FieldMissing path fn
                         | fn <- Map.keys codecFieldMap
                         , not (Map.member fn lexProps)
                         , fn /= "$type"  -- $type is injected by codecs, not in lexicon
                         ]
        -- Fields in lexicon but not in codec
        extraInLexicon = [FieldExtra path fn
                         | fn <- Map.keys lexProps
                         , not (Map.member fn codecFieldMap)
                         ]
        -- For each shared field, check type and requiredness
        sharedErrors   = concatMap (checkField path doc requiredSet codecFieldMap) (Map.toList lexProps)
    in  extraInCodec ++ extraInLexicon ++ sharedErrors

checkField :: Path -> LexiconDoc -> [T.Text] -> Map.Map T.Text LexField -> (T.Text, LexObjectProperty) -> [ValidationError]
checkField path doc requiredSet codecFieldMap (fieldN, lexProp) =
    case Map.lookup fieldN codecFieldMap of
      Nothing -> []  -- already reported as FieldExtra
      Just codecField ->
        let fieldPath       = path ++ [fieldN]
            lexRequired     = fieldN `elem` requiredSet
            codecRequired   = fieldRequired codecField
            reqErrors       = [RequiredMismatch fieldPath fieldN lexRequired codecRequired
                              | lexRequired /= codecRequired]
            codecFieldSchema = unwrapMaybe (fieldSchema codecField)
            typeErrors      = validateProperty fieldPath doc lexProp codecFieldSchema
        in  reqErrors ++ typeErrors
  where
    -- Optional fields have LexSchemaMaybe wrapper in codecs; peel it off
    unwrapMaybe (LexSchemaMaybe inner) = inner
    unwrapMaybe other                  = other

-- ---------------------------------------------------------------------------
-- Object property (field value types)
-- ---------------------------------------------------------------------------

validateProperty :: Path -> LexiconDoc -> LexObjectProperty -> LexSchema -> [ValidationError]
validateProperty path _doc (LexObjPrimBoolean _) s = matchPrimSchema path "boolean" LexSchemaBool s
validateProperty path _doc (LexObjPrimInteger _) s = matchPrimSchema path "integer" LexSchemaInt s
validateProperty path _doc (LexObjPrimString ls) s = validateStringProp path ls s
validateProperty path _doc (LexObjPrimUnknown _) s =
    case s of
      LexSchemaUnknown -> []
      _                -> [TypeMismatch path "unknown" (describeSchema s)]
validateProperty path _doc (LexObjIpldBytes _) s =
    case s of
      LexSchemaBytes -> []
      _              -> [TypeMismatch path "bytes" (describeSchema s)]
validateProperty path _doc (LexObjIpldCidLink _) s =
    case s of
      LexSchemaCid -> []
      _            -> [TypeMismatch path "cid-link" (describeSchema s)]
validateProperty path doc (LexObjRefSingle ref) s =
    validateRef path doc (lexRefRef ref) s
validateProperty path doc (LexObjRefUnion refU) s =
    validateRefUnion path doc refU s
validateProperty path _doc (LexObjBlob _) s =
    case s of
      LexSchemaBlob -> []
      _             -> [TypeMismatch path "blob" (describeSchema s)]
validateProperty path doc (LexObjArray arr) s =
    validateArray path doc arr s

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

validatePrimitive :: Path -> LexPrimitive -> LexSchema -> [ValidationError]
validatePrimitive path (LexPrimBoolean _) s = matchPrimSchema path "boolean" LexSchemaBool s
validatePrimitive path (LexPrimInteger _) s = matchPrimSchema path "integer" LexSchemaInt s
validatePrimitive path (LexPrimString ls) s = validateStringProp path ls s
validatePrimitive path (LexPrimUnknown _) s =
    case s of
      LexSchemaUnknown -> []
      _                -> [TypeMismatch path "unknown" (describeSchema s)]

matchPrimSchema :: Path -> T.Text -> LexSchema -> LexSchema -> [ValidationError]
matchPrimSchema path expected expectedS actual =
    if actual == expectedS
      then []
      else [TypeMismatch path expected (describeSchema actual)]

-- ---------------------------------------------------------------------------
-- String formats
-- ---------------------------------------------------------------------------

validateStringProp :: Path -> LexString -> LexSchema -> [ValidationError]
validateStringProp path lexStr codecSchema =
    case codecSchema of
      LexSchemaString codecFmt ->
        case (lexStrFormat lexStr, codecFmt) of
          -- Both have no format: OK
          (Nothing, Nothing) -> []
          -- Both have a format: compare
          (Just lexFmt, Just codFmt) ->
            if convertFormat lexFmt == codFmt then []
            else [TypeMismatch path
                    ("string/" <> formatName lexFmt)
                    ("string/" <> describeFormat codFmt)]
          -- Lexicon has format, codec doesn't: that's a mismatch
          (Just lexFmt, Nothing) ->
            [TypeMismatch path
              ("string/" <> formatName lexFmt)
              "string"]
          -- Codec has format, lexicon doesn't: this is fine
          -- (codec is more specific than lexicon requires)
          (Nothing, Just _) -> []
      _ -> [TypeMismatch path "string" (describeSchema codecSchema)]

-- | Convert a lexicon string format to the codec format type.
convertFormat :: LexStringFormat -> LexFormat
convertFormat LexFmtDatetime    = LexFormatDatetime
convertFormat LexFmtAtUri       = LexFormatAtUri
convertFormat LexFmtDid         = LexFormatDid
convertFormat LexFmtHandle      = LexFormatHandle
convertFormat LexFmtNsid        = LexFormatNsid
convertFormat LexFmtCid         = LexFormatCid
convertFormat LexFmtUri         = LexFormatUri
convertFormat LexFmtLanguage    = LexFormatLanguage
convertFormat LexFmtTid         = LexFormatTid
convertFormat LexFmtRecordKey   = LexFormatRecordKey
-- at-identifier is a super-type of did and handle; accept plain string
convertFormat LexFmtAtIdentifier = LexFormatDid

formatName :: LexStringFormat -> T.Text
formatName LexFmtDatetime     = "datetime"
formatName LexFmtUri          = "uri"
formatName LexFmtAtUri        = "at-uri"
formatName LexFmtDid          = "did"
formatName LexFmtHandle       = "handle"
formatName LexFmtAtIdentifier = "at-identifier"
formatName LexFmtNsid         = "nsid"
formatName LexFmtCid          = "cid"
formatName LexFmtLanguage     = "language"
formatName LexFmtTid          = "tid"
formatName LexFmtRecordKey    = "record-key"

-- ---------------------------------------------------------------------------
-- References
-- ---------------------------------------------------------------------------

validateRef :: Path -> LexiconDoc -> T.Text -> LexSchema -> [ValidationError]
validateRef path doc ref codecSchema =
    case codecSchema of
      -- If the codec uses a named ref, just check the names match
      LexSchemaRef codecRef ->
        if ref == codecRef then []
        else [TypeMismatch path ("ref:" <> ref) ("ref:" <> codecRef)]
      -- Otherwise, resolve the reference and validate structurally
      _ -> resolveAndValidate path doc ref codecSchema

validateRefUnion :: Path -> LexiconDoc -> LexRefUnion -> LexSchema -> [ValidationError]
validateRefUnion path _doc refU codecSchema =
    case codecSchema of
      LexSchemaUnion variants ->
        let lexRefs  = lexRefUnionRefs refU
            codecTags = map variantType variants
            -- Check that the set of union tags match
            missingInCodec  = [TypeMismatch path ("union ref:" <> r) "missing in codec union"
                              | r <- lexRefs, r `notElem` codecTags]
            extraInCodec    = [TypeMismatch path "extra in codec union" ("union ref:" <> t)
                              | t <- codecTags, t `notElem` lexRefs]
        in  missingInCodec ++ extraInCodec
      _ -> [TypeMismatch path "union" (describeSchema codecSchema)]

validateRefVariant :: Path -> LexiconDoc -> LexRefVariant -> LexSchema -> [ValidationError]
validateRefVariant path doc (LexRefSingle ref) s = validateRef path doc (lexRefRef ref) s
validateRefVariant path doc (LexRefUnionV refU) s = validateRefUnion path doc refU s

-- | Resolve a @#fragment@ or NSID reference and validate against it.
resolveAndValidate :: Path -> LexiconDoc -> T.Text -> LexSchema -> [ValidationError]
resolveAndValidate path doc ref codecSchema =
    case resolveRef doc ref of
      Nothing ->
        [DefinitionNotFound path ref]
      Just userType ->
        validateUserType path doc userType codecSchema

-- | Look up a reference in the lexicon document.
--
-- Handles @#fragment@ local references (looks up @fragment@ in defs)
-- and fully-qualified NSID references (not cross-document, but handles
-- the common @nsid#fragment@ form by extracting the fragment).
resolveRef :: LexiconDoc -> T.Text -> Maybe LexUserType
resolveRef doc ref
  | "#" `T.isPrefixOf` ref = Map.lookup (T.drop 1 ref) (lexDocDefs doc)
  | otherwise =
      -- Try as a direct def name, or extract fragment from nsid#fragment
      case T.breakOn "#" ref of
        (_, frag) | not (T.null frag) ->
          Map.lookup (T.drop 1 frag) (lexDocDefs doc)
        _ -> Map.lookup ref (lexDocDefs doc)

-- ---------------------------------------------------------------------------
-- Arrays
-- ---------------------------------------------------------------------------

validateArray :: Path -> LexiconDoc -> LexArray -> LexSchema -> [ValidationError]
validateArray path doc arr codecSchema =
    case codecSchema of
      LexSchemaArray inner ->
        validateProperty (path ++ ["items"]) doc (lexArrayItems arr) inner
      _ -> [TypeMismatch path "array" (describeSchema codecSchema)]

-- ---------------------------------------------------------------------------
-- Schema description (for error messages)
-- ---------------------------------------------------------------------------

describeSchema :: LexSchema -> T.Text
describeSchema LexSchemaNull         = "null"
describeSchema LexSchemaBool         = "boolean"
describeSchema LexSchemaInt          = "integer"
describeSchema (LexSchemaString mf)  = "string" <> maybe "" (\f -> "/" <> describeFormat f) mf
describeSchema LexSchemaBytes        = "bytes"
describeSchema LexSchemaCid          = "cid-link"
describeSchema LexSchemaBlob         = "blob"
describeSchema (LexSchemaArray _)    = "array"
describeSchema (LexSchemaObject _)   = "object"
describeSchema (LexSchemaUnion _)    = "union"
describeSchema (LexSchemaMaybe s)    = "maybe<" <> describeSchema s <> ">"
describeSchema (LexSchemaRef r)      = "ref:" <> r
describeSchema LexSchemaUnknown      = "unknown"

describeFormat :: LexFormat -> T.Text
describeFormat LexFormatDatetime  = "datetime"
describeFormat LexFormatAtUri     = "at-uri"
describeFormat LexFormatDid       = "did"
describeFormat LexFormatHandle    = "handle"
describeFormat LexFormatNsid      = "nsid"
describeFormat LexFormatCid       = "cid"
describeFormat LexFormatUri       = "uri"
describeFormat LexFormatLanguage  = "language"
describeFormat LexFormatTid       = "tid"
describeFormat LexFormatRecordKey = "record-key"
