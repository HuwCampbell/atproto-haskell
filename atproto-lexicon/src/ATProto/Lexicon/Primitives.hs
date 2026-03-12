-- | Lexicon primitive type definitions.
--
-- Port of the primitive type portion of \@atproto\/lexicon.
module ATProto.Lexicon.Primitives
  ( -- * Primitive types
    LexBoolean (..)
  , LexInteger (..)
  , LexStringFormat (..)
  , LexString (..)
  , LexUnknown (..)
  , LexPrimitive (..)
  ) where

import qualified Data.Text as T

-- | A boolean field definition.
data LexBoolean = LexBoolean
  { lexBoolDescription :: Maybe T.Text
  , lexBoolDefault     :: Maybe Bool
  , lexBoolConst       :: Maybe Bool
  } deriving (Eq, Show)

-- | An integer field definition.
data LexInteger = LexInteger
  { lexIntDescription :: Maybe T.Text
  , lexIntDefault     :: Maybe Int
  , lexIntMinimum     :: Maybe Int
  , lexIntMaximum     :: Maybe Int
  , lexIntEnum        :: Maybe [Int]
  , lexIntConst       :: Maybe Int
  } deriving (Eq, Show)

-- | Well-known string format constraints.
data LexStringFormat
  = LexFmtDatetime
  | LexFmtUri
  | LexFmtAtUri
  | LexFmtDid
  | LexFmtHandle
  | LexFmtAtIdentifier
  | LexFmtNsid
  | LexFmtCid
  | LexFmtLanguage
  | LexFmtTid
  | LexFmtRecordKey
  deriving (Eq, Ord, Show)

-- | A string field definition.
data LexString = LexString
  { lexStrFormat      :: Maybe LexStringFormat
  , lexStrDescription :: Maybe T.Text
  , lexStrDefault     :: Maybe T.Text
  , lexStrMinLength   :: Maybe Int
  , lexStrMaxLength   :: Maybe Int
  , lexStrMinGraphemes :: Maybe Int
  , lexStrMaxGraphemes :: Maybe Int
  , lexStrEnum        :: Maybe [T.Text]
  , lexStrConst       :: Maybe T.Text
  , lexStrKnownValues :: Maybe [T.Text]
  } deriving (Eq, Show)

-- | An unknown / opaque field definition.
newtype LexUnknown = LexUnknown
  { lexUnknownDescription :: Maybe T.Text
  } deriving (Eq, Show)

-- | The union of all primitive type definitions.
data LexPrimitive
  = LexPrimBoolean LexBoolean
  | LexPrimInteger LexInteger
  | LexPrimString  LexString
  | LexPrimUnknown LexUnknown
  deriving (Eq, Show)
