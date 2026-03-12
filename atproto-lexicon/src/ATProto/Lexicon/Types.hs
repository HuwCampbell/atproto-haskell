-- | Lexicon schema type definitions.
--
-- Port of \@atproto\/lexicon – the full set of Lexicon schema types used to
-- describe AT Protocol records, queries, procedures, and subscriptions.
module ATProto.Lexicon.Types
  ( -- * Re-exports from sub-modules
    module ATProto.Lexicon.Primitives
    -- * IPLD types
  , LexBytes (..)
  , LexCidLink (..)
  , LexIpldType (..)
    -- * References
  , LexRef (..)
  , LexRefUnion (..)
  , LexRefVariant (..)
    -- * Blob
  , LexBlob (..)
    -- * Complex types
  , LexArray (..)
  , LexToken (..)
  , LexObject (..)
  , LexObjectProperty (..)
    -- * User type
  , LexXrpcParameters (..)
  , LexXrpcBody (..)
  , LexXrpcError (..)
  , LexXrpcQuery (..)
  , LexXrpcProcedure (..)
  , LexXrpcSubscription (..)
  , LexRecord (..)
    -- * Top-level definitions
  , LexUserType (..)
  , LexiconDoc (..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

import ATProto.Lexicon.Primitives

-- ---------------------------------------------------------------------------
-- IPLD types
-- ---------------------------------------------------------------------------

-- | A bytes field definition (raw byte array).
data LexBytes = LexBytes
  { lexBytesDescription :: Maybe T.Text
  , lexBytesMinLength   :: Maybe Int
  , lexBytesMaxLength   :: Maybe Int
  } deriving (Eq, Show)

-- | A CID-link field definition (IPLD link).
newtype LexCidLink = LexCidLink
  { lexCidLinkDescription :: Maybe T.Text
  } deriving (Eq, Show)

-- | The union of IPLD type definitions.
data LexIpldType
  = LexIpldBytes   LexBytes
  | LexIpldCidLink LexCidLink
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- References
-- ---------------------------------------------------------------------------

-- | A single-target type reference.
data LexRef = LexRef
  { lexRefDescription :: Maybe T.Text
  , lexRefRef         :: T.Text
  } deriving (Eq, Show)

-- | A union-of-types reference.
data LexRefUnion = LexRefUnion
  { lexRefUnionDescription :: Maybe T.Text
  , lexRefUnionRefs        :: [T.Text]
  , lexRefUnionClosed      :: Maybe Bool
  } deriving (Eq, Show)

-- | Either a single reference or a union reference.
data LexRefVariant
  = LexRefSingle LexRef
  | LexRefUnionV LexRefUnion
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Blob
-- ---------------------------------------------------------------------------

-- | A blob field definition (file attachment).
data LexBlob = LexBlob
  { lexBlobDescription :: Maybe T.Text
  , lexBlobAccept      :: Maybe [T.Text]
  , lexBlobMaxSize     :: Maybe Int
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Complex types
-- ---------------------------------------------------------------------------

-- | Items that can appear as array elements or object property values.
data LexObjectProperty
  = LexObjPrimBoolean LexBoolean
  | LexObjPrimInteger LexInteger
  | LexObjPrimString  LexString
  | LexObjPrimUnknown LexUnknown
  | LexObjIpldBytes   LexBytes
  | LexObjIpldCidLink LexCidLink
  | LexObjRefSingle   LexRef
  | LexObjRefUnion    LexRefUnion
  | LexObjBlob        LexBlob
  | LexObjArray       LexArray
  deriving (Eq, Show)

-- | An array field definition.
data LexArray = LexArray
  { lexArrayDescription :: Maybe T.Text
  , lexArrayItems       :: LexObjectProperty
  , lexArrayMinLength   :: Maybe Int
  , lexArrayMaxLength   :: Maybe Int
  } deriving (Eq, Show)

-- | A token (named constant) type.
newtype LexToken = LexToken
  { lexTokenDescription :: Maybe T.Text
  } deriving (Eq, Show)

-- | An object type definition.
data LexObject = LexObject
  { lexObjDescription :: Maybe T.Text
  , lexObjRequired    :: Maybe [T.Text]
  , lexObjNullable    :: Maybe [T.Text]
  , lexObjProperties  :: Map.Map T.Text LexObjectProperty
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- XRPC schemas
-- ---------------------------------------------------------------------------

-- | Query-string parameters for an XRPC method.
data LexXrpcParameters = LexXrpcParameters
  { lexXrpcParamsDescription  :: Maybe T.Text
  , lexXrpcParamsRequired     :: Maybe [T.Text]
  , lexXrpcParamsProperties   :: Map.Map T.Text LexPrimitive
  } deriving (Eq, Show)

-- | A typed request or response body for an XRPC method.
data LexXrpcBody = LexXrpcBody
  { lexXrpcBodyDescription :: Maybe T.Text
  , lexXrpcBodyEncoding    :: T.Text
  , lexXrpcBodySchema      :: Maybe (Either LexObject LexRefVariant)
  } deriving (Eq, Show)

-- | An error definition for an XRPC method.
data LexXrpcError = LexXrpcError
  { lexXrpcErrorName        :: T.Text
  , lexXrpcErrorDescription :: Maybe T.Text
  } deriving (Eq, Show)

-- | An XRPC query (HTTP GET) definition.
data LexXrpcQuery = LexXrpcQuery
  { lexXrpcQueryDescription :: Maybe T.Text
  , lexXrpcQueryParameters  :: Maybe LexXrpcParameters
  , lexXrpcQueryOutput      :: Maybe LexXrpcBody
  , lexXrpcQueryErrors      :: Maybe [LexXrpcError]
  } deriving (Eq, Show)

-- | An XRPC procedure (HTTP POST) definition.
data LexXrpcProcedure = LexXrpcProcedure
  { lexXrpcProcDescription :: Maybe T.Text
  , lexXrpcProcParameters  :: Maybe LexXrpcParameters
  , lexXrpcProcInput       :: Maybe LexXrpcBody
  , lexXrpcProcOutput      :: Maybe LexXrpcBody
  , lexXrpcProcErrors      :: Maybe [LexXrpcError]
  } deriving (Eq, Show)

-- | An XRPC subscription (WebSocket event stream) definition.
data LexXrpcSubscription = LexXrpcSubscription
  { lexXrpcSubDescription :: Maybe T.Text
  , lexXrpcSubParameters  :: Maybe LexXrpcParameters
  , lexXrpcSubMessage     :: Maybe LexXrpcBody
  , lexXrpcSubErrors      :: Maybe [LexXrpcError]
  } deriving (Eq, Show)

-- | A Lexicon record type definition.
data LexRecord = LexRecord
  { lexRecordDescription :: Maybe T.Text
  , lexRecordKey         :: T.Text
  , lexRecordRecord      :: LexObject
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Top-level definitions
-- ---------------------------------------------------------------------------

-- | The discriminated union of all top-level Lexicon user types.
data LexUserType
  = LexRecord'       LexRecord
  | LexQuery         LexXrpcQuery
  | LexProcedure     LexXrpcProcedure
  | LexSubscription  LexXrpcSubscription
  | LexToken'        LexToken
  | LexObject'       LexObject
  | LexString'       LexString
  | LexBytes'        LexBytes
  | LexCidLink'      LexCidLink
  | LexBlob'         LexBlob
  | LexArray'        LexArray
  deriving (Eq, Show)

-- | A complete Lexicon document.
data LexiconDoc = LexiconDoc
  { lexDocLexicon     :: Int
    -- ^ Schema version, currently always @1@.
  , lexDocId          :: T.Text
    -- ^ The NSID that identifies this document.
  , lexDocRevision    :: Maybe Int
    -- ^ Optional revision counter.
  , lexDocDescription :: Maybe T.Text
    -- ^ Human-readable description.
  , lexDocDefs        :: Map.Map T.Text LexUserType
    -- ^ Named type definitions; @\"main\"@ is the primary entry-point.
  } deriving (Eq, Show)
