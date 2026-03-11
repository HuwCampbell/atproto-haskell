-- | AT Protocol Namespaced Identifier (NSID) syntax.
--
-- Implements the validation rules from the AT Protocol specification:
-- <https://atproto.com/specs/nsid>
--
-- An NSID is a reversed domain authority followed by a camel-case name,
-- e.g. @app.bsky.feed.getTimeline@.  The grammar is:
--
-- > nsid      = authority "." name
-- > authority = segment *("." segment)
-- > segment   = alpha *( alpha / number / "-" )
-- > name      = alpha *( alpha / number )
module ATProto.Syntax.NSID
  ( -- * Types
    NSID
  , nsidSegments
  , nsidAuthority
  , nsidName
    -- * Construction
  , parseNSID
  , mkNSID
  , isValidNSID
  ) where

import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Text as T

-- | An opaque, validated NSID value.
--
-- The internal representation stores the dot-separated segments in
-- left-to-right order (as they appear in the string), e.g.
-- @[\"app\",\"bsky\",\"feed\",\"getTimeline\"]@.
data NSID = NSID
  { nsidSegments :: [T.Text]
    -- ^ All dot-separated segments, left to right.
  } deriving (Eq, Ord)

instance Show NSID where
  show nsid = "NSID " ++ show (nsidToText nsid)

-- | Reconstruct the canonical string form.
nsidToText :: NSID -> T.Text
nsidToText (NSID segs) = T.intercalate "." segs

-- | The reversed-domain authority portion, e.g. @\"bsky.app\"@ for
-- @app.bsky.feed.getTimeline@.
nsidAuthority :: NSID -> T.Text
nsidAuthority (NSID segs) =
  T.intercalate "." . reverse $ init segs

-- | The final name component (camel-case), e.g. @\"getTimeline\"@.
nsidName :: NSID -> T.Text
nsidName (NSID segs) = last segs

-- | Parse and validate a raw text value as an 'NSID'.
parseNSID :: T.Text -> Either String NSID
parseNSID t
  | T.length t > 317    = Left "NSID is too long (317 chars max)"
  | not (T.all allowed t) = Left "Disallowed characters in NSID (ASCII letters, digits, dashes, periods only)"
  | otherwise =
      let segs = T.splitOn "." t
      in if length segs < 3
         then Left "NSID needs at least three parts"
         else do
           mapM_ checkSegment segs
           case segs of
             []    -> Left "NSID first part may not be empty"
             (s:_) -> checkFirstSeg s
           checkName (last segs)
           return (NSID segs)
  where
    allowed c = isAlphaNum c || c == '-' || c == '.'

    checkSegment s
      | T.null s        = Left "NSID parts can not be empty"
      | T.length s > 63 = Left "NSID part too long (max 63 chars)"
      | T.head s == '-'
        || T.last s == '-' = Left "NSID parts can not start or end with hyphen"
      | otherwise        = Right ()

    checkFirstSeg s
      | T.null s          = Left "NSID first part may not be empty"
      | not (isAlpha (T.head s)) = Left "NSID first part may not start with a digit"
      | otherwise          = Right ()

    checkName n
      | not (isAlpha (T.head n)) = Left "NSID name part must be only letters and digits (and no leading digit)"
      | T.any (== '-') n         = Left "NSID name part must be only letters and digits (and no leading digit)"
      | otherwise                = Right ()

-- | Construct an NSID from a reversed authority and a name.
--
-- @mkNSID \"bsky.app\" \"getTimeline\"@ produces the NSID
-- @app.bsky.getTimeline@.
mkNSID :: T.Text -> T.Text -> Either String NSID
mkNSID authority name =
  let parts = reverse (T.splitOn "." authority) ++ [name]
  in parseNSID (T.intercalate "." parts)

-- | Return 'True' when the text is a syntactically valid NSID.
isValidNSID :: T.Text -> Bool
isValidNSID t = case parseNSID t of
  Right _ -> True
  Left _  -> False
