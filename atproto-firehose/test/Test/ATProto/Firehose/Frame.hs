module Test.ATProto.Firehose.Frame (tests) where

import Hedgehog
import qualified Data.ByteString as BS
import qualified Data.Text       as T

import ATProto.Firehose.Events
import ATProto.Firehose.Frame

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A hand-crafted #commit frame (151 bytes).
--
-- Header: { op: 1, t: "#commit" }
-- Body:   { seq: 42, repo: "did:plc:test", commit: tag42(cid), rev: "rev1",
--           since: null, blocks: bytes([]), ops: [], tooBig: false,
--           time: "2024-01-01T00:00:00Z" }
commitFrame :: BS.ByteString
commitFrame = BS.pack
  [ 0xa2, 0x62, 0x6f, 0x70, 0x01, 0x61, 0x74, 0x67, 0x23, 0x63, 0x6f, 0x6d, 0x6d, 0x69, 0x74, 0xa9
  , 0x63, 0x73, 0x65, 0x71, 0x18, 0x2a, 0x64, 0x72, 0x65, 0x70, 0x6f, 0x6c, 0x64, 0x69, 0x64, 0x3a
  , 0x70, 0x6c, 0x63, 0x3a, 0x74, 0x65, 0x73, 0x74, 0x66, 0x63, 0x6f, 0x6d, 0x6d, 0x69, 0x74, 0xd8
  , 0x2a, 0x58, 0x25, 0x00, 0x01, 0x71, 0x12, 0x20, 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b
  , 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde, 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21
  , 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13, 0x63, 0x72, 0x65, 0x76, 0x64, 0x72, 0x65, 0x76
  , 0x31, 0x65, 0x73, 0x69, 0x6e, 0x63, 0x65, 0xf6, 0x66, 0x62, 0x6c, 0x6f, 0x63, 0x6b, 0x73, 0x40
  , 0x63, 0x6f, 0x70, 0x73, 0x80, 0x66, 0x74, 0x6f, 0x6f, 0x42, 0x69, 0x67, 0xf4, 0x64, 0x74, 0x69
  , 0x6d, 0x65, 0x74, 0x32, 0x30, 0x32, 0x34, 0x2d, 0x30, 0x31, 0x2d, 0x30, 0x31, 0x54, 0x30, 0x30
  , 0x3a, 0x30, 0x30, 0x3a, 0x30, 0x30, 0x5a
  ]

-- | A hand-crafted #identity frame.
--
-- Header: { op: 1, t: "#identity" }
-- Body:   { seq: 43, did: "did:plc:user", handle: "alice.bsky.social",
--           time: "2024-01-01T00:00:00Z" }
identityFrame :: BS.ByteString
identityFrame = BS.pack
  [ 0xa2, 0x62, 0x6f, 0x70, 0x01, 0x61, 0x74, 0x69, 0x23, 0x69, 0x64, 0x65, 0x6e, 0x74, 0x69, 0x74
  , 0x79, 0xa4, 0x63, 0x73, 0x65, 0x71, 0x18, 0x2b, 0x63, 0x64, 0x69, 0x64, 0x6c, 0x64, 0x69, 0x64
  , 0x3a, 0x70, 0x6c, 0x63, 0x3a, 0x75, 0x73, 0x65, 0x72, 0x66, 0x68, 0x61, 0x6e, 0x64, 0x6c, 0x65
  , 0x71, 0x61, 0x6c, 0x69, 0x63, 0x65, 0x2e, 0x62, 0x73, 0x6b, 0x79, 0x2e, 0x73, 0x6f, 0x63, 0x69
  , 0x61, 0x6c, 0x64, 0x74, 0x69, 0x6d, 0x65, 0x74, 0x32, 0x30, 0x32, 0x34, 0x2d, 0x30, 0x31, 0x2d
  , 0x30, 0x31, 0x54, 0x30, 0x30, 0x3a, 0x30, 0x30, 0x3a, 0x30, 0x30, 0x5a
  ]

-- | A hand-crafted frame with an unknown type ("#future").
unknownFrame :: BS.ByteString
unknownFrame = BS.pack
  [ 0xa2, 0x62, 0x6f, 0x70, 0x01, 0x61, 0x74, 0x67, 0x23, 0x66, 0x75, 0x74, 0x75, 0x72, 0x65, 0xa1
  , 0x63, 0x73, 0x65, 0x71, 0x18, 0x2c
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Decoding a valid #commit frame produces a 'FECommit' event.
prop_decodeCommit :: Property
prop_decodeCommit = property $ do
  case decodeFrame commitFrame of
    Left err           -> do
      annotate (show err)
      failure
    Right (FECommit ce) -> do
      ceSeq  ce === 42
      ceRepo ce === "did:plc:test"
      ceRev  ce === "rev1"
      ceSince ce === Nothing
      ceTooBig ce === False
      ceTime ce === "2024-01-01T00:00:00Z"
    Right other -> do
      annotate ("expected FECommit, got " ++ show other)
      failure

-- | The commit CID text field starts with 'b' (multibase base32lower).
prop_commitCidText :: Property
prop_commitCidText = property $ do
  case decodeFrame commitFrame of
    Right (FECommit ce) ->
      case T.uncons (ceCommit ce) of
        Just ('b', _) -> success
        _             -> failure
    _ -> failure

-- | Decoding a valid #identity frame produces a 'FEIdentity' event.
prop_decodeIdentity :: Property
prop_decodeIdentity = property $ do
  case decodeFrame identityFrame of
    Left err              -> do
      annotate (show err)
      failure
    Right (FEIdentity ie) -> do
      ieSeq    ie === 43
      ieDid    ie === "did:plc:user"
      ieHandle ie === Just "alice.bsky.social"
      ieTime   ie === "2024-01-01T00:00:00Z"
    Right other -> do
      annotate ("expected FEIdentity, got " ++ show other)
      failure

-- | An unknown event type produces 'FEUnknown'.
prop_decodeUnknown :: Property
prop_decodeUnknown = property $ do
  case decodeFrame unknownFrame of
    Right (FEUnknown t) -> t === "#future"
    Right other         -> do
      annotate ("expected FEUnknown, got " ++ show other)
      failure
    Left err -> do
      annotate (show err)
      failure

-- | Truncated input produces 'Left'.
prop_truncatedInput :: Property
prop_truncatedInput = property $ do
  let trunc = BS.take 5 commitFrame
  case decodeFrame trunc of
    Left _  -> success
    Right _ -> failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.Firehose.Frame"
  [ ("decode #commit frame",            prop_decodeCommit)
  , ("commit CID encoded as multibase", prop_commitCidText)
  , ("decode #identity frame",          prop_decodeIdentity)
  , ("unknown type → FEUnknown",        prop_decodeUnknown)
  , ("truncated input → Left",          prop_truncatedInput)
  ]
