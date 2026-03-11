-- | AT Protocol record key syntax validation.
--
-- Implements the validation rules from the AT Protocol specification:
-- <https://atproto.com/specs/record-key>
--
-- Record keys:
--
-- * consist of ASCII alphanumerics and the characters @._:~-@
-- * are 1–512 characters long
-- * may not be the literal strings @.@ or @..@
module ATProto.Syntax.RecordKey
  ( -- * Types
    RecordKey
  , unRecordKey
    -- * Validation
  , parseRecordKey
  , isValidRecordKey
  ) where

import qualified Data.Text as T

-- | An opaque, validated record key string.
newtype RecordKey = RecordKey { unRecordKey :: T.Text }
  deriving (Eq, Ord, Show)

recordKeyMinLength :: Int
recordKeyMinLength = 1

recordKeyMaxLength :: Int
recordKeyMaxLength = 512

-- | Characters allowed in a record key.
allowedChar :: Char -> Bool
allowedChar c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9') ||
  c `elem` ("_~.:-" :: String)

-- | Validate and wrap a 'T.Text' value as a 'RecordKey'.
parseRecordKey :: T.Text -> Either String RecordKey
parseRecordKey t
  | T.length t < recordKeyMinLength
    || T.length t > recordKeyMaxLength
                           = Left ("record key must be " ++ show recordKeyMinLength
                                   ++ " to " ++ show recordKeyMaxLength ++ " characters")
  | t == "." || t == ".." = Left "record key can not be \".\" or \"..\""
  | not (T.all allowedChar t) = Left "record key syntax not valid (regex)"
  | otherwise              = Right (RecordKey t)

-- | Return 'True' when the text is a syntactically valid record key.
isValidRecordKey :: T.Text -> Bool
isValidRecordKey t = case parseRecordKey t of
  Right _ -> True
  Left _  -> False
