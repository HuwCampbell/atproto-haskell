-- | MST point lookup.
--
-- 'get' traverses a Merkle Search Tree stored in a 'BlockMap', looking up
-- the 'CidBytes' associated with the given key.  The key should be in the
-- form @\"collection\/rkey\"@.
module ATProto.MST.Get
  ( get
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Text.Encoding as TE

import ATProto.Car.Cid      (CidBytes (..))
import ATProto.Car.BlockMap (BlockMap)
import ATProto.MST.Node     (NodeData (..), TreeEntry (..), decodeNode)
import ATProto.MST.Types    (MstError (..))

-- ---------------------------------------------------------------------------
-- Flat entry list
-- ---------------------------------------------------------------------------

-- | An element in the flattened node representation.
data FlatEntry
  = FLeaf T.Text CidBytes     -- ^ fully-expanded key, leaf value CID
  | FTree CidBytes            -- ^ subtree pointer
  deriving (Show)

-- | Expand a 'NodeData' into an ordered flat list of subtrees and leaves,
-- reconstructing full keys from the shared-prefix encoding.
-- The flat list interleaves: [Maybe(leftTree), leaf, Maybe(rightTree), ...]
flattenNode :: NodeData -> [FlatEntry]
flattenNode (NodeData mLeft entries) =
  maybe [] (\c -> [FTree c]) mLeft ++
  go "" entries
  where
    go _ []         = []
    go prev (te:rest) =
      let prevBytes = TE.encodeUtf8 prev
          shared    = BS.take (tePrefix te) prevBytes
          fullKey   = TE.decodeUtf8 (shared <> teSuffix te)
      in FLeaf fullKey (teValue te) :
         maybe [] (\c -> [FTree c]) (teRightTree te) ++
         go fullKey rest

-- ---------------------------------------------------------------------------
-- Point lookup
-- ---------------------------------------------------------------------------

-- | Look up a key in the MST rooted at @rootCid@.
--
-- Returns @Right (Just cid)@ if the key is present, @Right Nothing@ if
-- absent, or @Left err@ if a node could not be decoded.
get :: BlockMap -> CidBytes -> T.Text -> Either MstError (Maybe CidBytes)
get bmap rootCid key = getNode bmap rootCid key

getNode :: BlockMap -> CidBytes -> T.Text -> Either MstError (Maybe CidBytes)
getNode bmap cid key = do
  raw      <- lookupBlock bmap cid
  nodeData <- mapLeft (MstDecodeError . T.pack) (decodeNode raw)
  let flat = flattenNode nodeData
  search bmap flat key

search :: BlockMap -> [FlatEntry] -> T.Text -> Either MstError (Maybe CidBytes)
search bmap flat key = go flat
  where
    go [] = Right Nothing
    go (FLeaf k v : rest)
      | k == key  = Right (Just v)
      | k >  key  = Right Nothing   -- past the key, not found
      | otherwise = go rest
    go (FTree cid : rest) =
      -- Check if the subtree might contain the key by peeking at the next leaf.
      let nextLeafKey = firstLeafKey rest
      in if maybe True (\nk -> key < nk) nextLeafKey
           then getNode bmap cid key  -- descend into subtree
           else go rest               -- skip subtree, key is to the right

-- | Return the key of the first leaf in a flat list (if any).
firstLeafKey :: [FlatEntry] -> Maybe T.Text
firstLeafKey []              = Nothing
firstLeafKey (FLeaf k _ : _) = Just k
firstLeafKey (FTree _ : rest) = firstLeafKey rest

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lookupBlock :: BlockMap -> CidBytes -> Either MstError BS.ByteString
lookupBlock bmap cid =
  case Map.lookup cid bmap of
    Nothing  -> Left (MstNodeNotFound (T.pack (show (unCidBytes cid))))
    Just raw -> Right raw

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left e)  = Left (f e)
mapLeft _ (Right x) = Right x
