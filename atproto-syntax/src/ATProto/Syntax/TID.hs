-- | AT Protocol TID (Timestamp Identifier) syntax validation.
--
-- Implements the validation rules from the AT Protocol specification:
-- <https://atproto.com/specs/tid>
--
-- A TID is exactly 13 characters in base32-sortable encoding.  The
-- first character is drawn from @[234567abcdefghij]@ (so the high bit
-- of the underlying @int64@ is never set), and the remaining 12
-- characters are from the full base32-sortable alphabet
-- @[234567abcdefghijklmnopqrstuvwxyz]@.
module ATProto.Syntax.TID
  ( -- * Types
    TID
  , unTID
    -- * Validation
  , parseTID
  , isValidTID
  ) where

import qualified Data.Text as T

-- | An opaque, validated TID string.
newtype TID = TID { unTID :: T.Text }
  deriving (Eq, Ord, Show)

tidLength :: Int
tidLength = 13

-- | Valid first-character set for a TID (high-bit-safe in base32-sortable).
validFirstChars :: String
validFirstChars = "234567abcdefghij"

-- | Full base32-sortable alphabet.
base32SortableAlphabet :: String
base32SortableAlphabet = "234567abcdefghijklmnopqrstuvwxyz"

-- | Validate and wrap a 'T.Text' value as a 'TID'.
parseTID :: T.Text -> Either String TID
parseTID t
  | T.length t /= tidLength             = Left ("TID must be " ++ show tidLength ++ " characters")
  | T.head t `notElem` validFirstChars  = Left "TID syntax not valid (first character out of range)"
  | not (T.all (`elem` base32SortableAlphabet) t) = Left "TID syntax not valid (regex)"
  | otherwise                           = Right (TID t)

-- | Return 'True' when the text is a syntactically valid TID.
isValidTID :: T.Text -> Bool
isValidTID t = case parseTID t of
  Right _ -> True
  Left _  -> False
