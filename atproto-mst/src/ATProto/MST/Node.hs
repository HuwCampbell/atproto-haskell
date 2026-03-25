-- | MST node CBOR decoding.
--
-- An MST node is stored as a DAG-CBOR map with two fields:
--
--   * @l@ – optional CID pointing to the leftmost subtree
--   * @e@ – list of 'TreeEntry' values
--
-- Each 'TreeEntry' represents one key/value pair plus an optional right
-- subtree pointer.  The full key for entry @i@ is reconstructed by taking
-- the first @p@ bytes of the previous fully-expanded key and appending the
-- bytes in @k@.
module ATProto.MST.Node
  ( -- * Types
    NodeData (..)
  , TreeEntry (..)
    -- * Decoding
  , decodeNode
  ) where

import qualified Data.ByteString      as BS
import qualified Codec.CBOR.Decoding  as D
import qualified Codec.CBOR.Read      as R
import qualified Data.ByteString.Lazy as BL
import           Data.Word            (Word64)

import ATProto.Car.Cid (CidBytes (..), parseCidFromBytes)

-- | An MST node as decoded from a CAR block.
data NodeData = NodeData
  { nodeLeft    :: Maybe CidBytes   -- ^ leftmost subtree CID, if any
  , nodeEntries :: [TreeEntry]      -- ^ ordered list of entries
  } deriving (Eq, Show)

-- | A single entry in an MST node.
data TreeEntry = TreeEntry
  { tePrefix     :: Int              -- ^ shared-prefix length with previous key
  , teSuffix     :: BS.ByteString    -- ^ remaining key bytes after shared prefix
  , teValue      :: CidBytes         -- ^ CID of the leaf record
  , teRightTree  :: Maybe CidBytes   -- ^ optional right-subtree CID
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- CBOR decoding
-- ---------------------------------------------------------------------------

-- | Decode an MST node from raw DAG-CBOR bytes.
decodeNode :: BS.ByteString -> Either String NodeData
decodeNode raw =
  case R.deserialiseFromBytes nodeDecoder (BL.fromStrict raw) of
    Left  err      -> Left (show err)
    Right (_, nd)  -> Right nd

nodeDecoder :: D.Decoder s NodeData
nodeDecoder = do
  mapLen <- D.decodeMapLen
  parseNodeMap mapLen Nothing Nothing

parseNodeMap
  :: Int
  -> Maybe (Maybe CidBytes)  -- l field (outer Maybe = seen, inner = nullable)
  -> Maybe [TreeEntry]
  -> D.Decoder s NodeData
parseNodeMap 0 mL mE = do
  let leftmost = case mL of
                   Nothing       -> Nothing
                   Just Nothing  -> Nothing
                   Just (Just c) -> Just c
  entries <- case mE of
               Nothing -> fail "MST node missing 'e' field"
               Just es -> return es
  return (NodeData leftmost entries)
parseNodeMap n mL mE = do
  key <- D.decodeString
  case key of
    "l" -> do
      cid <- decodeNullableTag42
      parseNodeMap (n - 1) (Just cid) mE
    "e" -> do
      len     <- D.decodeListLen
      entries <- mapM (\_ -> decodeTreeEntry) [1..len]
      parseNodeMap (n - 1) mL (Just entries)
    _   -> do
      -- Skip unknown field value
      _ <- D.decodeNull
      parseNodeMap (n - 1) mL mE

decodeTreeEntry :: D.Decoder s TreeEntry
decodeTreeEntry = do
  mapLen <- D.decodeMapLen
  parseEntryMap mapLen Nothing Nothing Nothing Nothing

parseEntryMap
  :: Int
  -> Maybe Word64           -- p
  -> Maybe BS.ByteString    -- k
  -> Maybe CidBytes         -- v
  -> Maybe (Maybe CidBytes) -- t
  -> D.Decoder s TreeEntry
parseEntryMap 0 mP mK mV mT = do
  p <- case mP of
         Nothing -> fail "TreeEntry missing 'p'"
         Just x  -> return (fromIntegral x)
  k <- case mK of
         Nothing -> fail "TreeEntry missing 'k'"
         Just x  -> return x
  v <- case mV of
         Nothing -> fail "TreeEntry missing 'v'"
         Just x  -> return x
  let t = case mT of
            Nothing       -> Nothing
            Just Nothing  -> Nothing
            Just (Just c) -> Just c
  return (TreeEntry p k v t)
parseEntryMap n mP mK mV mT = do
  key <- D.decodeString
  case key of
    "p" -> do
      pv <- fromIntegral <$> D.decodeWord
      parseEntryMap (n - 1) (Just pv) mK mV mT
    "k" -> do
      kv <- D.decodeBytes
      parseEntryMap (n - 1) mP (Just kv) mV mT
    "v" -> do
      cid <- decodeTag42
      parseEntryMap (n - 1) mP mK (Just cid) mT
    "t" -> do
      cid <- decodeNullableTag42
      parseEntryMap (n - 1) mP mK mV (Just cid)
    _   -> do
      _ <- D.decodeNull
      parseEntryMap (n - 1) mP mK mV mT

-- | Decode a tag-42 CID (non-nullable).
decodeTag42 :: D.Decoder s CidBytes
decodeTag42 = do
  tag <- D.decodeTag
  if tag /= 42
    then fail ("expected CBOR tag 42, got " ++ show tag)
    else do
      raw <- D.decodeBytes
      let stripped = if not (BS.null raw) && BS.head raw == 0x00
                       then BS.tail raw
                       else raw
      case parseCidFromBytes stripped 0 of
        Left err       -> fail err
        Right (cid, _) -> return cid

-- | Decode a nullable tag-42 CID (CBOR null or tag 42 bytes).
decodeNullableTag42 :: D.Decoder s (Maybe CidBytes)
decodeNullableTag42 = do
  ty <- D.peekTokenType
  case ty of
    D.TypeNull -> D.decodeNull >> return Nothing
    _          -> Just <$> decodeTag42
