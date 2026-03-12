-- | AT Protocol handle syntax validation.
--
-- Implements the validation rules from the AT Protocol specification:
-- <https://atproto.com/specs/handle>
--
-- A handle is a domain name, e.g. @alice.bsky.social@.  Only ASCII
-- letters, digits, hyphens, and periods are allowed.  The maximum
-- overall length is 253 characters; each label is 1–63 characters and
-- may not start or end with a hyphen; the TLD must begin with an ASCII
-- letter.
module ATProto.Syntax.Handle
  ( -- * Types
    Handle
  , unHandle
    -- * Constants
  , invalidHandle
    -- * Validation and normalisation
  , parseHandle
  , isValidHandle
  , normaliseHandle
  , parseAndNormaliseHandle
    -- * TLD policy
  , isDisallowedTld
  ) where

import Data.Char   (isAlpha, isAlphaNum, toLower)
import qualified Data.Text as T

-- | An opaque, validated handle string.
--
-- Handles are case-insensitive; they are stored as lower-case after
-- normalisation via 'normaliseHandle' / 'parseAndNormaliseHandle'.
newtype Handle = Handle { unHandle :: T.Text }
  deriving (Eq, Ord, Show)

-- | Sentinel value used in profile records when a handle cannot be resolved.
invalidHandle :: T.Text
invalidHandle = "handle.invalid"

-- | Top-level domains that are disallowed for registration (policy layer).
disallowedTlds :: [T.Text]
disallowedTlds =
  [ ".local"
  , ".arpa"
  , ".invalid"
  , ".localhost"
  , ".internal"
  , ".example"
  , ".alt"
  , ".onion"
  ]

-- | Return 'True' if the given handle ends with a disallowed TLD.
isDisallowedTld :: Handle -> Bool
isDisallowedTld (Handle h) =
  any (`T.isSuffixOf` h) disallowedTlds

-- | Validate a raw text value and wrap it as a 'Handle'.
--
-- The input is NOT normalised (lower-cased); use 'parseAndNormaliseHandle'
-- when you want validation and normalisation in one step.
parseHandle :: T.Text -> Either String Handle
parseHandle t
  | not (T.all allowedChar t)  = Left "Disallowed characters in handle (ASCII letters, digits, dashes, periods only)"
  | T.length t > 253           = Left "Handle is too long (253 chars max)"
  | otherwise                  =
      let labels = T.splitOn "." t
      in case labels of
           []  -> Left "Handle domain needs at least two parts"
           [_] -> Left "Handle domain needs at least two parts"
           ls  -> mapM_ checkLabel (zip [0 :: Int ..] ls) >> Right (Handle t)
  where
    allowedChar c = isAlphaNum c || c == '-' || c == '.'

    checkLabel (idx, l)
      | T.null l            = Left "Handle parts can not be empty"
      | T.length l > 63     = Left "Handle part too long (max 63 chars)"
      | T.head l == '-'
        || T.last l == '-'  = Left "Handle parts can not start or end with hyphens"
      | isTld && not (isAlpha (T.head l))
                             = Left "Handle final component (TLD) must start with ASCII letter"
      | otherwise            = Right ()
      where isTld = idx + 1 == length (T.splitOn "." t) -- last label

-- | Return 'True' when the text is a syntactically valid handle.
isValidHandle :: T.Text -> Bool
isValidHandle t = case parseHandle t of
  Right _ -> True
  Left _  -> False

-- | Normalise a 'Handle' to lower-case (handles are case-insensitive).
normaliseHandle :: Handle -> Handle
normaliseHandle (Handle h) = Handle (T.map toLower h)

-- | Normalise and validate in one step, returning a lower-cased 'Handle'.
parseAndNormaliseHandle :: T.Text -> Either String Handle
parseAndNormaliseHandle t = do
  h <- parseHandle (T.map toLower t)
  return (normaliseHandle h)
