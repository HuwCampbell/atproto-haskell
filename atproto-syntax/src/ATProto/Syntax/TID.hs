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

  , tidNow
  ) where

import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Text (Text)
import           System.Clock (getTime, Clock(..), toNanoSecs)
import           System.Random (randomRIO)

-- | An opaque, validated TID string.
newtype TID = TID { unTID :: Text }
  deriving (Eq, Ord, Show)

tidLength :: Int
tidLength = 13

-- | Valid first-character set for a TID (high-bit-safe in base32-sortable).
validFirstChars :: String
validFirstChars = "234567abcdefghij"

-- | Full base32-sortable alphabet.
base32SortableAlphabet :: String
base32SortableAlphabet = "234567abcdefghijklmnopqrstuvwxyz"

-- | Validate and wrap a 'Text' value as a 'TID'.
parseTID :: Text -> Either String TID
parseTID t
  | Text.length t /= tidLength             = Left ("TID must be " ++ show tidLength ++ " characters")
  | Text.head t `notElem` validFirstChars  = Left "TID syntax not valid (first character out of range)"
  | not (Text.all (`elem` base32SortableAlphabet) t) = Left "TID syntax not valid (regex)"
  | otherwise                           = Right (TID t)

-- | Return 'True' when the text is a syntactically valid TID.
isValidTID :: Text -> Bool
isValidTID t = case parseTID t of
  Right _ -> True
  Left _  -> False

-- | Base32-sortable alphabet (used by AT Protocol TIDs)
encodeBase32 :: Integral i => i -> Text
encodeBase32 top =
  let
    to j =
      Char.chr . fromIntegral $
        if j < 6 then
          j + 50
        else
          j + 91

    expander j =
      if j == 0 then
        Nothing
      else let
        (rest, c) = j `divMod` 32
      in
        Just ( to c, rest )
  in
  Text.reverse $
    Text.unfoldr expander top


getPosixMicros :: IO Integer
getPosixMicros = do
  t <- getTime Realtime
  return $ fromIntegral (toNanoSecs t `div` 1000)


-- | Generate a TID from the current time (microseconds since epoch).
tidNow :: IO TID
tidNow = do
  micros  <- getPosixMicros
  clockId <- randomRIO (0, 31 :: Int)
  let
    timePart =
      padTextLeft 11 '2' $
        encodeBase32 micros

    clockPart =
      padTextLeft 2 '2' $
        encodeBase32 clockId

  -- High bit must be 0 (ensured because posix micros fit in 63 bits for centuries)
  return $ TID $ timePart <> clockPart

padTextLeft :: Int -> Char -> Text -> Text
padTextLeft n c txt =
  let len = Text.length txt
      pad = max 0 (n - len)
  in Text.replicate pad (Text.singleton c) <> txt
