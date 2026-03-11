-- | AT Protocol AT URI syntax.
--
-- Implements parsing and validation of AT URIs as described in:
-- <https://atproto.com/specs/at-uri-scheme>
--
-- An AT URI has the form:
--
-- @at:\/\/\<authority\>[\/\<collection\>[\/\<rkey\>]]@
--
-- where \<authority\> is either a DID or a handle.
module ATProto.Syntax.AtUri
  ( -- * Types
    AtUri (..)
    -- * Construction
  , parseAtUri
  , isValidAtUri
    -- * Rendering
  , renderAtUri
  ) where

import Data.Char (isAlphaNum)
import qualified Data.Text as T

import ATProto.Syntax.DID       (isValidDID)
import ATProto.Syntax.Handle    (isValidHandle)
import ATProto.Syntax.NSID      (isValidNSID)

-- | A parsed AT URI.
data AtUri = AtUri
  { atUriAuthority   :: T.Text
    -- ^ The authority: a DID or a handle.
  , atUriCollection  :: Maybe T.Text
    -- ^ Optional collection NSID.
  , atUriRkey        :: Maybe T.Text
    -- ^ Optional record key (only present when 'atUriCollection' is present).
  } deriving (Eq, Show)

-- | Parse and validate an AT URI string.
--
-- Both the @at:\/\/@ prefix form and the bare form (without scheme) are
-- accepted.
parseAtUri :: T.Text -> Either String AtUri
parseAtUri raw = do
  -- strip optional scheme
  let t = if "at://" `T.isPrefixOf` raw
            then T.drop 5 raw
            else raw
  -- split off query / fragment (not currently stored)
  let (pathPart, _rest) = T.breakOn "?" (T.takeWhile (/= '#') t)
  -- split path
  let parts = filter (not . T.null) (T.splitOn "/" pathPart)
  case parts of
    []          -> Left "AT URI requires an authority"
    (auth:rest) -> do
      validateAuthority auth
      (col, rk) <- case rest of
        []        -> return (Nothing, Nothing)
        [c]       -> do
          validateCollection c
          return (Just c, Nothing)
        [c, r]    -> do
          validateCollection c
          validateRkey r
          return (Just c, Just r)
        _         -> Left "AT URI path has too many segments (max: authority/collection/rkey)"
      return AtUri
        { atUriAuthority  = auth
        , atUriCollection = col
        , atUriRkey       = rk
        }
  where
    validateAuthority a
      | isValidDID a    = Right ()
      | isValidHandle a = Right ()
      | T.null a        = Left "AT URI authority must not be empty"
      | otherwise       = Left "AT URI authority is neither a valid DID nor a valid handle"

    validateCollection c
      | isValidNSID c = Right ()
      | otherwise     = Left ("AT URI collection is not a valid NSID: " ++ T.unpack c)

    validateRkey r
      | T.all allowedRkeyChar r && not (T.null r)
                      = Right ()
      | otherwise     = Left ("AT URI rkey contains disallowed characters: " ++ T.unpack r)

    allowedRkeyChar c = isAlphaNum c || c `elem` ("._:~!$&'()*+,;=-@" :: String)

-- | Return 'True' when the text is a syntactically valid AT URI.
isValidAtUri :: T.Text -> Bool
isValidAtUri t = case parseAtUri t of
  Right _ -> True
  Left _  -> False

-- | Render an 'AtUri' back to its canonical string form.
renderAtUri :: AtUri -> T.Text
renderAtUri AtUri{atUriAuthority = auth, atUriCollection = col, atUriRkey = rk} =
  T.concat $ ["at://", auth] ++ colPart ++ rkPart
  where
    colPart = maybe [] (\c -> ["/" , c]) col
    rkPart  = maybe [] (\r -> ["/", r]) rk
