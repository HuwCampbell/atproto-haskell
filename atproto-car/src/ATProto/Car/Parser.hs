-- | CAR v1 binary format parser.
--
-- The CAR v1 format consists of:
--
-- 1. A varint giving the byte length of the header block.
-- 2. The header block: a DAG-CBOR map @{ version: 1, roots: [CID, ...] }@
--    where each CID is encoded as CBOR tag 42 with bytes @\\x00 ++ raw_cid@.
-- 3. Zero or more data blocks, each preceded by a varint giving
--    @len(cid_bytes) + len(block_bytes)@, followed by the raw CID bytes and
--    the block bytes.
module ATProto.Car.Parser
  ( -- * Errors
    CarError (..)
    -- * Parsers
  , readCar
  , readCarWithRoot
  ) where

import           Data.Bits            ((.&.), (.|.), shiftL)
import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Codec.CBOR.Decoding  as D
import qualified Codec.CBOR.Read      as R
import qualified Data.ByteString.Lazy as BL

import           ATProto.Car.Cid      (CidBytes (..), parseCidFromBytes, cidForDagCbor)
import           ATProto.Car.BlockMap (BlockMap)
import           ATProto.Car.DagCbor  (decodeCidTag42)
import Control.Monad (unless)

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

-- | Errors that can occur while reading a CAR file.
data CarError
  = CarBadVarint
    -- ^ A varint could not be decoded.
  | CarBadHeader T.Text
    -- ^ The header block could not be parsed.
  | CarNoRoot
    -- ^ The header contains zero roots.
  | CarMultipleRoots
    -- ^ The header contains more than one root (use 'readCar' for multi-root).
  | CarBadCid T.Text
    -- ^ A CID could not be parsed.
  | CarBadBlock T.Text
    -- ^ A block entry is malformed.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Varint decoding
-- ---------------------------------------------------------------------------

-- | Decode an unsigned LEB-128 varint from a 'BS.ByteString' at @offset@.
-- Returns @(value, bytesConsumed)@.
decodeVarint :: BS.ByteString -> Int -> Either CarError (Int, Int)
decodeVarint bs start = loop 0 0 start
  where
    loop :: Int -> Int -> Int -> Either CarError (Int, Int)
    loop acc shift pos
      | pos >= BS.length bs = Left CarBadVarint
      | otherwise =
          let b   = fromIntegral (BS.index bs pos) :: Int
              val = acc .|. ((b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
             then Right (val, pos - start + 1)
             else loop val (shift + 7) (pos + 1)

-- ---------------------------------------------------------------------------
-- Header parsing
-- ---------------------------------------------------------------------------

-- | Parse the CAR header using CBOR and return the list of root CIDs.
parseHeader :: BS.ByteString -> Either CarError [CidBytes]
parseHeader hdrBytes =
  case R.deserialiseFromBytes decodeHeader (BL.fromStrict hdrBytes) of
    Left  err        -> Left (CarBadHeader (T.pack (show err)))
    Right (_, cids)  -> Right cids
  where
    decodeHeader :: D.Decoder s [CidBytes]
    decodeHeader = do
      n <- D.decodeMapLen
      parseMap n Nothing

    parseMap :: Int -> Maybe [CidBytes] -> D.Decoder s [CidBytes]
    parseMap 0 mRoots =
      case mRoots of
        Nothing -> fail "CAR header missing 'roots' field"
        Just rs -> return rs
    parseMap n mRoots = do
      key <- D.decodeString
      case key of
        "version" -> do
          _ver <- D.decodeWord
          parseMap (n - 1) mRoots
        "roots" -> do
          len  <- D.decodeListLen
          cids <- mapM (\_ -> decodeCidTag42) [1..len]
          parseMap (n - 1) (Just cids)
        _ -> do
          -- Skip unknown field value with a best-effort approach
          _ <- D.decodeNull
          parseMap (n - 1) mRoots

-- ---------------------------------------------------------------------------
-- Block sequence parsing
-- ---------------------------------------------------------------------------

-- | Parse all data blocks from @offset@ to end-of-input.
parseBlocks :: BS.ByteString -> Int -> Either CarError BlockMap -> Either CarError BlockMap
parseBlocks bs off accE
  | off >= BS.length bs = accE
  | otherwise = do
      acc <- accE
      (entryLen, vLen) <- decodeVarint bs off
      let off1 = off + vLen
      if off1 + entryLen > BS.length bs
        then Left (CarBadBlock "block entry extends beyond input")
        else do
          let entry = BS.take entryLen (BS.drop off1 bs)
          (cid, cidLen) <-
            case parseCidFromBytes entry 0 of
              Left  err       -> Left (CarBadCid (T.pack err))
              Right (c, n)    -> Right (c, n)
          let blockBytes = BS.drop cidLen entry
              expectedCid = cidForDagCbor blockBytes   -- recompute from content

          --
          -- Critical security check, if we omit this, then it's possible that
          -- a malicious actor could sneak in a CAR file into a trusted boundary
          -- which contains unverified information.
          unless (cid == expectedCid) $
            Left (CarBadBlock "CID does not match block content")

          parseBlocks bs (off1 + entryLen) (Right (Map.insert cid blockBytes acc))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Parse a CAR v1 byte string.
--
-- Returns the list of root CIDs (may be zero or more) and the block map.
readCar :: BS.ByteString -> Either CarError ([CidBytes], BlockMap)
readCar bs = do
  (hdrLen, vLen) <- decodeVarint bs 0
  if vLen + hdrLen > BS.length bs
    then Left (CarBadHeader "header length exceeds input")
    else do
      let hdrBytes  = BS.take hdrLen (BS.drop vLen bs)
          blocksOff = vLen + hdrLen
      roots  <- parseHeader hdrBytes
      blocks <- parseBlocks bs blocksOff (Right Map.empty)
      return (roots, blocks)

-- | Parse a CAR v1 byte string that must have exactly one root CID.
--
-- Returns the single root CID and the block map.
readCarWithRoot :: BS.ByteString -> Either CarError (CidBytes, BlockMap)
readCarWithRoot bs = do
  (roots, blocks) <- readCar bs
  case roots of
    []  -> Left CarNoRoot
    [r] -> Right (r, blocks)
    _   -> Left CarMultipleRoots
