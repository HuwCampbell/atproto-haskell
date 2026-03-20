module Test.ATProto.Car.Parser (tests) where

import Hedgehog
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import ATProto.Car.Cid    (CidBytes (..), cidToText)
import ATProto.Car.Parser

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A minimal hand-crafted CAR v1 file containing one block.
--
-- Block bytes:  0xf6 (CBOR null)
-- Block CID:    dag-cbor sha2-256 of [0xf6]
--               = 01711220b0b2988b6bbe724bacda5e9e524736de0bc7dae41c46b4213c50e1d35d4e5f13
--
-- CAR layout:
--   varint(58)   -- header byte length
--   header CBOR  -- { version: 1, roots: [tag42(cid)] }
--   varint(37)   -- entry length = 36 (cid) + 1 (block)
--   cid bytes    -- 36 bytes
--   block bytes  -- 1 byte (0xf6)
minimalCar :: BS.ByteString
minimalCar = BS.pack
  [ 0x3a  -- varint: header is 58 bytes
  -- header: CBOR map { "version": 1, "roots": [tag42(cid)] }
  , 0xa2
  , 0x67, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e  -- text(7) "version"
  , 0x01                                               -- 1
  , 0x65, 0x72, 0x6f, 0x6f, 0x74, 0x73               -- text(5) "roots"
  , 0x81                                               -- array(1)
  -- tag(42) bytes(37): \x00 ++ cid_bytes
  , 0xd8, 0x2a, 0x58, 0x25, 0x00
  -- CID bytes (36 bytes):
  , 0x01, 0x71, 0x12, 0x20
  , 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b
  , 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde
  , 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21
  , 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13
  -- entry: varint(37)
  , 0x25
  -- CID bytes (36 bytes):
  , 0x01, 0x71, 0x12, 0x20
  , 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b
  , 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde
  , 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21
  , 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13
  -- block bytes: CBOR null
  , 0xf6
  ]

-- | The expected CID of the block in 'minimalCar'.
expectedCidBytes :: BS.ByteString
expectedCidBytes = BS.pack
  [ 0x01, 0x71, 0x12, 0x20
  , 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b
  , 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde
  , 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21
  , 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13
  ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | 'readCar' succeeds on the minimal fixture.
prop_readCarOk :: Property
prop_readCarOk = property $ do
  case readCar minimalCar of
    Left err -> do
      annotate (show err)
      failure
    Right (roots, blocks) -> do
      length roots === 1
      Map.size blocks === 1

-- | 'readCarWithRoot' returns the expected single root CID.
prop_readCarWithRootCid :: Property
prop_readCarWithRootCid = property $ do
  case readCarWithRoot minimalCar of
    Left err      -> do
      annotate (show err)
      failure
    Right (cid, _) ->
      unCidBytes cid === expectedCidBytes

-- | The block map contains the expected block bytes.
prop_blockMapContents :: Property
prop_blockMapContents = property $ do
  case readCarWithRoot minimalCar of
    Left err           -> do
      annotate (show err)
      failure
    Right (cid, blocks) ->
      Map.lookup cid blocks === Just (BS.singleton 0xf6)

-- | CID text representation matches base32lower multibase.
prop_cidTextRepresentation :: Property
prop_cidTextRepresentation = property $ do
  case readCarWithRoot minimalCar of
    Left err       -> do
      annotate (show err)
      failure
    Right (cid, _) ->
      cidToText cid === "bafyreifqwkmiw256ojf2zws6tzjeonw6bpd5vza4i22ccpcq4hjv2ts7cm"

-- | Truncated input yields an error.
prop_truncatedInput :: Property
prop_truncatedInput = property $ do
  let truncated = BS.take 30 minimalCar
  case readCar truncated of
    Left _  -> success
    Right _ -> failure

-- | A CAR with zero roots yields 'CarNoRoot' from 'readCarWithRoot'.
prop_zeroRoots :: Property
prop_zeroRoots = property $ do
  -- Build a CAR whose header has an empty roots array.
  let hdr = BS.pack
        [ 0xa2
        , 0x67, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e
        , 0x01
        , 0x65, 0x72, 0x6f, 0x6f, 0x74, 0x73
        , 0x80  -- array(0)
        ]
      hdrLen = BS.length hdr  -- 17
      -- varint for 17 = 0x11
      car = BS.singleton (fromIntegral hdrLen) <> hdr
  case readCarWithRoot car of
    Left CarNoRoot -> success
    other          -> do
      annotate (show other)
      failure

-- | A CAR with multiple roots yields 'CarMultipleRoots' from 'readCarWithRoot'.
prop_multipleRoots :: Property
prop_multipleRoots = property $ do
  -- Duplicate the root entry: array(2) with two copies of the same CID.
  let cidTag = BS.pack
        [ 0xd8, 0x2a, 0x58, 0x25, 0x00
        , 0x01, 0x71, 0x12, 0x20
        , 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b
        , 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde
        , 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21
        , 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13
        ]
      hdr = BS.pack
              [ 0xa2
              , 0x67, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e
              , 0x01
              , 0x65, 0x72, 0x6f, 0x6f, 0x74, 0x73
              , 0x82  -- array(2)
              ]
            <> cidTag <> cidTag
      hdrLen = BS.length hdr
      car = BS.singleton (fromIntegral hdrLen) <> hdr
  case readCarWithRoot car of
    Left CarMultipleRoots -> success
    other                 -> do
      annotate (show other)
      failure

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.Car.Parser"
  [ ("readCar succeeds on minimal fixture",      prop_readCarOk)
  , ("readCarWithRoot returns correct CID",      prop_readCarWithRootCid)
  , ("block map contains expected bytes",        prop_blockMapContents)
  , ("CID text representation is correct",       prop_cidTextRepresentation)
  , ("truncated input yields error",             prop_truncatedInput)
  , ("zero roots yields CarNoRoot",              prop_zeroRoots)
  , ("multiple roots yields CarMultipleRoots",   prop_multipleRoots)
  ]
