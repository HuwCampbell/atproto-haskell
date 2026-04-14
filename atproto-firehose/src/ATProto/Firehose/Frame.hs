-- | Firehose frame decoding.
--
-- Each WebSocket binary message is a DAG-CBOR sequence of two maps:
--
-- 1. Header map: @{ op: int, t: string }@
--    * @op = 1@  – normal message
--    * @op = -1@ – error frame
-- 2. Body map: the event-specific fields
--
-- CID fields in the body carry CBOR tag 42 bytes (@\\x00 ++ raw_cid@),
-- which are decoded and re-encoded as multibase base32lower text for
-- compatibility with the string-based 'ATProto.Ipld.Value.Cid' type.
module ATProto.Firehose.Frame
  ( -- * Errors
    FrameError (..)
    -- * Decoder
  , decodeFrame
  ) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text              as T
import qualified Codec.CBOR.Decoding    as D
import qualified Codec.CBOR.Read        as R

import ATProto.Car.Cid       (CidBytes (..), parseCidFromBytes, cidToText)
import ATProto.Firehose.Events

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

-- | Errors that can occur when decoding a firehose frame.
data FrameError
  = FrameDecodeError String
    -- ^ CBOR decoding failed.
  | FrameUnknownOp Int
    -- ^ The frame header contained an unrecognised @op@ value.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Decode a WebSocket binary message into a 'FirehoseEvent'.
decodeFrame :: BS.ByteString -> Either FrameError FirehoseEvent
decodeFrame bs =
  case R.deserialiseFromBytes frameDecoder (BL.fromStrict bs) of
    Left  err      -> Left (FrameDecodeError (show err))
    Right (_, evt) -> Right evt

-- ---------------------------------------------------------------------------
-- CBOR frame decoder
-- ---------------------------------------------------------------------------

frameDecoder :: D.Decoder s FirehoseEvent
frameDecoder = do
  -- The message is two concatenated CBOR values: header map then body map.
  (op, mType) <- decodeHeader
  case op of
    1  -> decodeBody mType
    -1 -> do
      -- Error frame: decode as InfoEvent with name "error"
      msg <- decodeErrorBody
      return (FEInfo (InfoEvent "error" (Just msg)))
    n  -> fail ("unknown frame op: " ++ show n)

-- | Decode the header map: @{ op: int, t: string }@
decodeHeader :: D.Decoder s (Int, Maybe T.Text)
decodeHeader = do
  n <- D.decodeMapLen
  parseHeader n Nothing Nothing

parseHeader :: Int -> Maybe Int -> Maybe T.Text -> D.Decoder s (Int, Maybe T.Text)
parseHeader 0 mOp mType =
  case mOp of
    Nothing -> fail "frame header missing 'op'"
    Just op -> return (op, mType)
parseHeader n mOp mType = do
  key <- D.decodeString
  case key of
    "op" -> do
      ty <- D.peekTokenType
      op <- case ty of
        D.TypeUInt -> fromIntegral <$> D.decodeWord
        D.TypeNInt -> negate . fromIntegral . (1+) <$> D.decodeNegWord
        _          -> fail "frame header 'op' is not an integer"
      parseHeader (n-1) (Just op) mType
    "t"  -> do
      t <- D.decodeString
      parseHeader (n-1) mOp (Just t)
    _    -> do
      skipCborValue
      parseHeader (n-1) mOp mType

-- | Dispatch on the @t@ (type) field.
decodeBody :: Maybe T.Text -> D.Decoder s FirehoseEvent
decodeBody Nothing = fail "frame header missing 't' field"
decodeBody (Just t) =
  case t of
    "#commit"   -> FECommit   <$> decodeCommitBody
    "#identity" -> FEIdentity <$> decodeIdentityBody
    "#account"  -> FEAccount  <$> decodeAccountBody
    "#sync"     -> FESync     <$> decodeSyncBody
    "#info"     -> FEInfo     <$> decodeInfoBody
    _           -> do
      -- Skip the body map, return FEUnknown
      skipCborValue
      return (FEUnknown t)

-- ---------------------------------------------------------------------------
-- Body decoders
-- ---------------------------------------------------------------------------

decodeCommitBody :: D.Decoder s CommitEvent
decodeCommitBody = do
  n <- D.decodeMapLen
  go n (CommitEvent 0 "" "" "" Nothing BS.empty [] False "")
  where
    go 0 e = return e
    go n e = do
      key <- D.decodeString
      e'  <- case key of
        "seq"    -> (\v -> e { ceSeq    = fromIntegral v }) <$> D.decodeWord
        "repo"   -> (\v -> e { ceRepo   = v })              <$> D.decodeString
        "commit" -> (\v -> e { ceCommit = v })              <$> decodeCidTag42OrString
        "rev"    -> (\v -> e { ceRev    = v })              <$> D.decodeString
        "since"  -> (\v -> e { ceSince  = v })              <$> decodeNullableString
        "blocks" -> (\v -> e { ceBlocks = v })              <$> D.decodeBytes
        "ops"    -> (\v -> e { ceOps    = v })              <$> decodeRepoOps
        "tooBig" -> (\v -> e { ceTooBig = v })              <$> D.decodeBool
        "time"   -> (\v -> e { ceTime   = v })              <$> D.decodeString
        _        -> skipCborValue >> return e
      go (n-1) e'

decodeIdentityBody :: D.Decoder s IdentityEvent
decodeIdentityBody = do
  n <- D.decodeMapLen
  go n (IdentityEvent 0 "" Nothing "")
  where
    go 0 e = return e
    go n e = do
      key <- D.decodeString
      e'  <- case key of
        "seq"    -> (\v -> e { ieSeq    = fromIntegral v }) <$> D.decodeWord
        "did"    -> (\v -> e { ieDid    = v })              <$> D.decodeString
        "handle" -> (\v -> e { ieHandle = v })              <$> decodeNullableString
        "time"   -> (\v -> e { ieTime   = v })              <$> D.decodeString
        _        -> skipCborValue >> return e
      go (n-1) e'

decodeAccountBody :: D.Decoder s AccountEvent
decodeAccountBody = do
  n <- D.decodeMapLen
  go n (AccountEvent 0 "" False Nothing "")
  where
    go 0 e = return e
    go n e = do
      key <- D.decodeString
      e'  <- case key of
        "seq"    -> (\v -> e { aeSeq    = fromIntegral v }) <$> D.decodeWord
        "did"    -> (\v -> e { aeDid    = v })              <$> D.decodeString
        "active" -> (\v -> e { aeActive = v })              <$> D.decodeBool
        "status" -> (\v -> e { aeStatus = v })              <$> decodeNullableString
        "time"   -> (\v -> e { aeTime   = v })              <$> D.decodeString
        _        -> skipCborValue >> return e
      go (n-1) e'

decodeSyncBody :: D.Decoder s SyncEvent
decodeSyncBody = do
  n <- D.decodeMapLen
  go n (SyncEvent 0 "" "" BS.empty "")
  where
    go 0 e = return e
    go n e = do
      key <- D.decodeString
      e'  <- case key of
        "seq"    -> (\v -> e { seSeq    = fromIntegral v }) <$> D.decodeWord
        "did"    -> (\v -> e { seDid    = v })              <$> D.decodeString
        "rev"    -> (\v -> e { seRev    = v })              <$> D.decodeString
        "blocks" -> (\v -> e { seBlocks = v })              <$> D.decodeBytes
        "time"   -> (\v -> e { seTime   = v })              <$> D.decodeString
        _        -> skipCborValue >> return e
      go (n-1) e'

decodeInfoBody :: D.Decoder s InfoEvent
decodeInfoBody = do
  n <- D.decodeMapLen
  go n (InfoEvent "" Nothing)
  where
    go 0 e = return e
    go n e = do
      key <- D.decodeString
      e'  <- case key of
        "name"    -> (\v -> e { infoName    = v }) <$> D.decodeString
        "message" -> (\v -> e { infoMessage = v }) <$> decodeNullableString
        _         -> skipCborValue >> return e
      go (n-1) e'

decodeErrorBody :: D.Decoder s T.Text
decodeErrorBody = do
  n <- D.decodeMapLen
  go n ""
  where
    go 0 msg = return msg
    go n msg = do
      key <- D.decodeString
      case key of
        "message" -> D.decodeString >>= \v -> go (n-1) v
        _         -> skipCborValue  >> go (n-1) msg

decodeRepoOps :: D.Decoder s [RepoOp]
decodeRepoOps = do
  len <- D.decodeListLen
  mapM (\_ -> decodeRepoOp) [1..len]

decodeRepoOp :: D.Decoder s RepoOp
decodeRepoOp = do
  n <- D.decodeMapLen
  go n (RepoOp OpCreate "" Nothing Nothing)
  where
    go 0 e = return e
    go n e = do
      key <- D.decodeString
      e'  <- case key of
        "action" -> (\v -> e { ropAction = parseAction v }) <$> D.decodeString
        "path"   -> (\v -> e { ropPath   = v })             <$> D.decodeString
        "cid"    -> (\v -> e { ropCid    = v })             <$> decodeNullableCidText
        "prev"   -> (\v -> e { ropPrev   = v })             <$> decodeNullableCidText
        _        -> skipCborValue >> return e
      go (n-1) e'

parseAction :: T.Text -> OpAction
parseAction "create" = OpCreate
parseAction "update" = OpUpdate
parseAction "delete" = OpDelete
parseAction _        = OpCreate  -- default to create for unknown actions

-- ---------------------------------------------------------------------------
-- CID helpers
-- ---------------------------------------------------------------------------

-- | Decode a CBOR tag-42 CID and convert to multibase text, or decode a
-- plain string (for backward compat).
decodeCidTag42OrString :: D.Decoder s T.Text
decodeCidTag42OrString = do
  ty <- D.peekTokenType
  case ty of
    D.TypeTag    -> cidTagToText <$> decodeCidTag42Raw
    D.TypeString -> D.decodeString
    _            -> fail "expected CID (tag 42 or string)"

cidTagToText :: CidBytes -> T.Text
cidTagToText = cidToText

decodeCidTag42Raw :: D.Decoder s CidBytes
decodeCidTag42Raw = do
  tag <- D.decodeTag
  if tag /= 42
    then fail ("expected tag 42, got " ++ show tag)
    else do
      raw <- D.decodeBytes
      let stripped = if not (BS.null raw) && BS.head raw == 0x00
                       then BS.tail raw
                       else raw
      case parseCidFromBytes stripped 0 of
        Left  err      -> fail err
        Right (cid, _) -> return cid

decodeNullableCidText :: D.Decoder s (Maybe T.Text)
decodeNullableCidText = do
  ty <- D.peekTokenType
  case ty of
    D.TypeNull -> D.decodeNull >> return Nothing
    _          -> Just <$> decodeCidTag42OrString

decodeNullableString :: D.Decoder s (Maybe T.Text)
decodeNullableString = do
  ty <- D.peekTokenType
  case ty of
    D.TypeNull -> D.decodeNull >> return Nothing
    _          -> Just <$> D.decodeString

-- ---------------------------------------------------------------------------
-- Skip arbitrary CBOR value
-- ---------------------------------------------------------------------------

skipCborValue :: D.Decoder s ()
skipCborValue = do
  ty <- D.peekTokenType
  case ty of
    D.TypeNull    -> D.decodeNull
    D.TypeBool    -> D.decodeBool    >> return ()
    D.TypeUInt    -> D.decodeWord    >> return ()
    D.TypeNInt    -> D.decodeNegWord >> return ()
    D.TypeBytes   -> D.decodeBytes   >> return ()
    D.TypeString  -> D.decodeString  >> return ()
    D.TypeListLen -> do
      len <- D.decodeListLen
      mapM_ (\_ -> skipCborValue) [1..len]
    D.TypeMapLen  -> do
      len <- D.decodeMapLen
      mapM_ (\_ -> D.decodeString >> skipCborValue) [1..len]
    D.TypeTag     -> D.decodeTag >> skipCborValue
    D.TypeFloat64 -> D.decodeDouble >> return ()
    _             -> fail ("skipCborValue: unsupported token " ++ show ty)
