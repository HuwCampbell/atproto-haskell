-- | AT Protocol DID (Decentralised Identifier) syntax validation.
--
-- Implements the validation rules from the AT Protocol specification:
-- <https://atproto.com/specs/did>
--
-- A DID is a URI of the form @did:\<method\>:\<method-specific-id\>@.
-- Only ASCII letters, digits, and the characters @._:%-@ are allowed.
-- The maximum length is 2048 characters.
module ATProto.Syntax.DID
  ( -- * Types
    DID
  , unDID
    -- * Validation
  , parseDID
  , isValidDID
  ) where

import qualified Data.Text as T

-- | An opaque, validated DID string.
--
-- Construct with 'parseDID'; extract the underlying text with 'unDID'.
newtype DID = DID { unDID :: T.Text }
  deriving (Eq, Ord, Show)

-- | Validate and wrap a 'T.Text' value as a 'DID'.
--
-- Returns 'Left' with an error message when the input does not satisfy the
-- DID syntax constraints.
parseDID :: T.Text -> Either String DID
parseDID t
  | T.null t                        = Left "DID must not be empty"
  | T.length t > 2048               = Left "DID is too long (2048 chars max)"
  | not ("did:" `T.isPrefixOf` t)   = Left "DID requires \"did:\" prefix"
  | ":" `T.isSuffixOf` t            = Left "DID can not end with \":\""
  | "%" `T.isSuffixOf` t            = Left "DID can not end with \"%\""
  | not (T.all allowedChar t)       = Left "Disallowed characters in DID (ASCII letters, digits, and a couple other characters only)"
  | otherwise                       = case T.splitOn ":" t of
      (_:method:_rest)
        | T.null method              -> Left "DID method must not be empty"
        | not (T.all isLower method) -> Left "DID method must be lower-case letters"
        | otherwise                  -> Right (DID t)
      _                              -> Left "DID requires prefix, method, and method-specific content"
  where
    allowedChar c = isAsciiAlpha c || isDigit c || c `elem` ("._:%-" :: String)
    isAsciiAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    isDigit c      = c >= '0' && c <= '9'
    isLower c      = c >= 'a' && c <= 'z'

-- | Return 'True' when the text is a syntactically valid DID.
isValidDID :: T.Text -> Bool
isValidDID t = case parseDID t of
  Right _ -> True
  Left _  -> False
