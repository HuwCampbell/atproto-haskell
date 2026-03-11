-- | JSON parsing for Lexicon schema documents.
--
-- Provides 'Data.Aeson.FromJSON' instances for all Lexicon types so that
-- @.lexicon.json@ files can be decoded into 'LexiconDoc' values:
--
-- @
-- import ATProto.Lexicon
-- import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString.Lazy as BL
--
-- loadLexicon :: FilePath -> IO (Either String LexiconDoc)
-- loadLexicon path = Aeson.eitherDecode \<$\> BL.readFile path
-- @
--
-- All instances are written by hand – no Template Haskell is used.
module ATProto.Lexicon.Json () where

import           Control.Applicative  ((<|>))
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.Lexicon.Primitives
import ATProto.Lexicon.Types

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

instance Aeson.FromJSON LexBoolean where
  parseJSON = Aeson.withObject "LexBoolean" $ \o ->
    LexBoolean
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "default"
      <*> o Aeson..:? "const"

instance Aeson.FromJSON LexInteger where
  parseJSON = Aeson.withObject "LexInteger" $ \o ->
    LexInteger
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "default"
      <*> o Aeson..:? "minimum"
      <*> o Aeson..:? "maximum"
      <*> o Aeson..:? "enum"
      <*> o Aeson..:? "const"

instance Aeson.FromJSON LexStringFormat where
  parseJSON = Aeson.withText "LexStringFormat" $ \t ->
    case t of
      "datetime"      -> return LexFmtDatetime
      "uri"           -> return LexFmtUri
      "at-uri"        -> return LexFmtAtUri
      "did"           -> return LexFmtDid
      "handle"        -> return LexFmtHandle
      "at-identifier" -> return LexFmtAtIdentifier
      "nsid"          -> return LexFmtNsid
      "cid"           -> return LexFmtCid
      "language"      -> return LexFmtLanguage
      "tid"           -> return LexFmtTid
      "record-key"    -> return LexFmtRecordKey
      _               -> fail ("Unknown string format: " ++ T.unpack t)

instance Aeson.FromJSON LexString where
  parseJSON = Aeson.withObject "LexString" $ \o ->
    LexString
      <$> o Aeson..:? "format"
      <*> o Aeson..:? "description"
      <*> o Aeson..:? "default"
      <*> o Aeson..:? "minLength"
      <*> o Aeson..:? "maxLength"
      <*> o Aeson..:? "minGraphemes"
      <*> o Aeson..:? "maxGraphemes"
      <*> o Aeson..:? "enum"
      <*> o Aeson..:? "const"
      <*> o Aeson..:? "knownValues"

instance Aeson.FromJSON LexUnknown where
  parseJSON = Aeson.withObject "LexUnknown" $ \o ->
    LexUnknown <$> o Aeson..:? "description"

instance Aeson.FromJSON LexPrimitive where
  parseJSON = Aeson.withObject "LexPrimitive" $ \o -> do
    t <- o Aeson..: "type" :: Aeson.Parser T.Text
    case t of
      "boolean" -> LexPrimBoolean <$> Aeson.parseJSON (Aeson.Object o)
      "integer" -> LexPrimInteger <$> Aeson.parseJSON (Aeson.Object o)
      "string"  -> LexPrimString  <$> Aeson.parseJSON (Aeson.Object o)
      "unknown" -> LexPrimUnknown <$> Aeson.parseJSON (Aeson.Object o)
      _         -> fail ("Unknown primitive type: " ++ T.unpack t)

-- ---------------------------------------------------------------------------
-- IPLD types
-- ---------------------------------------------------------------------------

instance Aeson.FromJSON LexBytes where
  parseJSON = Aeson.withObject "LexBytes" $ \o ->
    LexBytes
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "minLength"
      <*> o Aeson..:? "maxLength"

instance Aeson.FromJSON LexCidLink where
  parseJSON = Aeson.withObject "LexCidLink" $ \o ->
    LexCidLink <$> o Aeson..:? "description"

instance Aeson.FromJSON LexIpldType where
  parseJSON = Aeson.withObject "LexIpldType" $ \o -> do
    t <- o Aeson..: "type" :: Aeson.Parser T.Text
    case t of
      "bytes"    -> LexIpldBytes   <$> Aeson.parseJSON (Aeson.Object o)
      "cid-link" -> LexIpldCidLink <$> Aeson.parseJSON (Aeson.Object o)
      _          -> fail ("Unknown IPLD type: " ++ T.unpack t)

-- ---------------------------------------------------------------------------
-- References
-- ---------------------------------------------------------------------------

instance Aeson.FromJSON LexRef where
  parseJSON = Aeson.withObject "LexRef" $ \o ->
    LexRef
      <$> o Aeson..:? "description"
      <*> o Aeson..:  "ref"

instance Aeson.FromJSON LexRefUnion where
  parseJSON = Aeson.withObject "LexRefUnion" $ \o ->
    LexRefUnion
      <$> o Aeson..:? "description"
      <*> o Aeson..:  "refs"
      <*> o Aeson..:? "closed"

instance Aeson.FromJSON LexRefVariant where
  parseJSON = Aeson.withObject "LexRefVariant" $ \o -> do
    t <- o Aeson..: "type" :: Aeson.Parser T.Text
    case t of
      "ref"   -> LexRefSingle <$> Aeson.parseJSON (Aeson.Object o)
      "union" -> LexRefUnionV <$> Aeson.parseJSON (Aeson.Object o)
      _       -> fail ("Unknown ref variant type: " ++ T.unpack t)

-- ---------------------------------------------------------------------------
-- Blob
-- ---------------------------------------------------------------------------

instance Aeson.FromJSON LexBlob where
  parseJSON = Aeson.withObject "LexBlob" $ \o ->
    LexBlob
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "accept"
      <*> o Aeson..:? "maxSize"

-- ---------------------------------------------------------------------------
-- Complex types
-- ---------------------------------------------------------------------------

instance Aeson.FromJSON LexObjectProperty where
  parseJSON = Aeson.withObject "LexObjectProperty" $ \o -> do
    t <- o Aeson..: "type" :: Aeson.Parser T.Text
    case t of
      "boolean"  -> LexObjPrimBoolean <$> Aeson.parseJSON (Aeson.Object o)
      "integer"  -> LexObjPrimInteger <$> Aeson.parseJSON (Aeson.Object o)
      "string"   -> LexObjPrimString  <$> Aeson.parseJSON (Aeson.Object o)
      "unknown"  -> LexObjPrimUnknown <$> Aeson.parseJSON (Aeson.Object o)
      "bytes"    -> LexObjIpldBytes   <$> Aeson.parseJSON (Aeson.Object o)
      "cid-link" -> LexObjIpldCidLink <$> Aeson.parseJSON (Aeson.Object o)
      "ref"      -> LexObjRefSingle   <$> Aeson.parseJSON (Aeson.Object o)
      "union"    -> LexObjRefUnion    <$> Aeson.parseJSON (Aeson.Object o)
      "blob"     -> LexObjBlob        <$> Aeson.parseJSON (Aeson.Object o)
      "array"    -> LexObjArray       <$> Aeson.parseJSON (Aeson.Object o)
      _          -> fail ("Unknown object property type: " ++ T.unpack t)

instance Aeson.FromJSON LexArray where
  parseJSON = Aeson.withObject "LexArray" $ \o ->
    LexArray
      <$> o Aeson..:? "description"
      <*> o Aeson..:  "items"
      <*> o Aeson..:? "minLength"
      <*> o Aeson..:? "maxLength"

instance Aeson.FromJSON LexToken where
  parseJSON = Aeson.withObject "LexToken" $ \o ->
    LexToken <$> o Aeson..:? "description"

instance Aeson.FromJSON LexObject where
  parseJSON = Aeson.withObject "LexObject" $ \o ->
    LexObject
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "required"
      <*> o Aeson..:? "nullable"
      <*> o Aeson..:? "properties" Aeson..!= Map.empty

-- ---------------------------------------------------------------------------
-- XRPC schemas
-- ---------------------------------------------------------------------------

instance Aeson.FromJSON LexXrpcParameters where
  parseJSON = Aeson.withObject "LexXrpcParameters" $ \o ->
    LexXrpcParameters
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "required"
      <*> o Aeson..:? "properties" Aeson..!= Map.empty

instance Aeson.FromJSON LexXrpcBody where
  parseJSON = Aeson.withObject "LexXrpcBody" $ \o -> do
    desc     <- o Aeson..:? "description"
    encoding <- o Aeson..:  "encoding"
    mSchema  <- o Aeson..:? "schema"
    schema   <- traverse parseBodySchema mSchema
    return (LexXrpcBody desc encoding schema)
    where
      parseBodySchema v =
            (Left  <$> Aeson.parseJSON v)
        <|> (Right <$> Aeson.parseJSON v)

instance Aeson.FromJSON LexXrpcError where
  parseJSON = Aeson.withObject "LexXrpcError" $ \o ->
    LexXrpcError
      <$> o Aeson..:  "name"
      <*> o Aeson..:? "description"

instance Aeson.FromJSON LexXrpcQuery where
  parseJSON = Aeson.withObject "LexXrpcQuery" $ \o ->
    LexXrpcQuery
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "parameters"
      <*> o Aeson..:? "output"
      <*> o Aeson..:? "errors"

instance Aeson.FromJSON LexXrpcProcedure where
  parseJSON = Aeson.withObject "LexXrpcProcedure" $ \o ->
    LexXrpcProcedure
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "parameters"
      <*> o Aeson..:? "input"
      <*> o Aeson..:? "output"
      <*> o Aeson..:? "errors"

instance Aeson.FromJSON LexXrpcSubscription where
  parseJSON = Aeson.withObject "LexXrpcSubscription" $ \o ->
    LexXrpcSubscription
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "parameters"
      <*> o Aeson..:? "message"
      <*> o Aeson..:? "errors"

instance Aeson.FromJSON LexRecord where
  parseJSON = Aeson.withObject "LexRecord" $ \o ->
    LexRecord
      <$> o Aeson..:? "description"
      <*> o Aeson..:? "key"    Aeson..!= "tid"
      <*> o Aeson..:  "record"

-- ---------------------------------------------------------------------------
-- Top-level definitions
-- ---------------------------------------------------------------------------

instance Aeson.FromJSON LexUserType where
  parseJSON = Aeson.withObject "LexUserType" $ \o -> do
    t <- o Aeson..: "type" :: Aeson.Parser T.Text
    case t of
      "record"       -> LexRecord'      <$> Aeson.parseJSON (Aeson.Object o)
      "query"        -> LexQuery        <$> Aeson.parseJSON (Aeson.Object o)
      "procedure"    -> LexProcedure    <$> Aeson.parseJSON (Aeson.Object o)
      "subscription" -> LexSubscription <$> Aeson.parseJSON (Aeson.Object o)
      "token"        -> LexToken'       <$> Aeson.parseJSON (Aeson.Object o)
      "object"       -> LexObject'      <$> Aeson.parseJSON (Aeson.Object o)
      "string"       -> LexString'      <$> Aeson.parseJSON (Aeson.Object o)
      "bytes"        -> LexBytes'       <$> Aeson.parseJSON (Aeson.Object o)
      "cid-link"     -> LexCidLink'     <$> Aeson.parseJSON (Aeson.Object o)
      "blob"         -> LexBlob'        <$> Aeson.parseJSON (Aeson.Object o)
      "array"        -> LexArray'       <$> Aeson.parseJSON (Aeson.Object o)
      _              -> fail ("Unknown lexicon type: " ++ T.unpack t)

instance Aeson.FromJSON LexiconDoc where
  parseJSON = Aeson.withObject "LexiconDoc" $ \o ->
    LexiconDoc
      <$> o Aeson..:  "lexicon"
      <*> o Aeson..:  "id"
      <*> o Aeson..:? "revision"
      <*> o Aeson..:? "description"
      <*> o Aeson..:? "defs" Aeson..!= Map.empty
