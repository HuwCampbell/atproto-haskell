-- | MST tree tests: lookup, diff, verify, and round-trip.
module Test.ATProto.MST.Tree (tests) where

import Hedgehog
import qualified Data.ByteString as BS

import ATProto.Car.Cid      (CidBytes (..), parseCidFromBytes)
import ATProto.Car.Parser   (readCarWithRoot)
import ATProto.MST.Tree
  ( WriteDescr (..), RecordOp (..)
  , fromBlockMap, toBlockMap
  , lookup, toList, verifyProofs, mstCid
  )
import qualified ATProto.MST.Tree as Tree

import Prelude hiding (lookup)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A single-entry MST CAR file.
--
-- The tree contains exactly one record:
--   key = "com.example.record/3jqfcqzm3fn2j"
--   val = CID of CBOR null block
--
-- Generated from the AT Protocol reference test vectors.
singleEntryCar :: BS.ByteString
singleEntryCar = BS.pack
  [ 0x3a, 0xa2, 0x67, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x01, 0x65, 0x72, 0x6f, 0x6f, 0x74
  , 0x73, 0x81, 0xd8, 0x2a, 0x58, 0x25, 0x00, 0x01, 0x71, 0x12, 0x20, 0xce, 0x9c, 0xd4, 0xc3, 0x61
  , 0x25, 0xff, 0x0d, 0x5a, 0x10, 0x88, 0x85, 0x2e, 0xdc, 0x44, 0x5f, 0xdc, 0x08, 0xc1, 0xaf, 0x98
  , 0xf5, 0x01, 0x4e, 0x49, 0xf1, 0x9b, 0x06, 0x59, 0x1e, 0xf0, 0x76, 0x81, 0x01, 0x01, 0x71, 0x12
  , 0x20, 0xce, 0x9c, 0xd4, 0xc3, 0x61, 0x25, 0xff, 0x0d, 0x5a, 0x10, 0x88, 0x85, 0x2e, 0xdc, 0x44
  , 0x5f, 0xdc, 0x08, 0xc1, 0xaf, 0x98, 0xf5, 0x01, 0x4e, 0x49, 0xf1, 0x9b, 0x06, 0x59, 0x1e, 0xf0
  , 0x76, 0xa2, 0x61, 0x6c, 0xf6, 0x61, 0x65, 0x81, 0xa4, 0x61, 0x70, 0x00, 0x61, 0x6b, 0x58, 0x20
  , 0x63, 0x6f, 0x6d, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2e, 0x72, 0x65, 0x63, 0x6f
  , 0x72, 0x64, 0x2f, 0x33, 0x6a, 0x71, 0x66, 0x63, 0x71, 0x7a, 0x6d, 0x33, 0x66, 0x6e, 0x32, 0x6a
  , 0x61, 0x76, 0xd8, 0x2a, 0x58, 0x25, 0x00, 0x01, 0x71, 0x12, 0x20, 0xb0, 0xb2, 0x98, 0x8b, 0x6b
  , 0xbe, 0x72, 0x4b, 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde, 0x0b, 0xc7, 0xda, 0xe4, 0x1c
  , 0x46, 0xb4, 0x21, 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13, 0x61, 0x74, 0xf6, 0x25, 0x01
  , 0x71, 0x12, 0x20, 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b, 0xac, 0xda, 0x5e, 0x9e, 0x52
  , 0x47, 0x36, 0xde, 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21, 0x3c, 0x50, 0xe1, 0xd3, 0x5d
  , 0x4e, 0x5f, 0x13, 0xf6
  ]

-- | Leaf CID bytes (sha256 of CBOR null).
leafCidBytes :: BS.ByteString
leafCidBytes = BS.pack
  [ 0x01, 0x71, 0x12, 0x20
  , 0xb0, 0xb2, 0x98, 0x8b, 0x6b, 0xbe, 0x72, 0x4b
  , 0xac, 0xda, 0x5e, 0x9e, 0x52, 0x47, 0x36, 0xde
  , 0x0b, 0xc7, 0xda, 0xe4, 0x1c, 0x46, 0xb4, 0x21
  , 0x3c, 0x50, 0xe1, 0xd3, 0x5d, 0x4e, 0x5f, 0x13
  ]

parsedLeafCid :: CidBytes
parsedLeafCid = case parseCidFromBytes leafCidBytes 0 of
  Right (c, _) -> c
  Left  err    -> error ("parsedLeafCid: " ++ err)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | 'lookup' returns the correct CID for a present key.
prop_getPresent :: Property
prop_getPresent = property $ do
  case readCarWithRoot singleEntryCar of
    Left err              -> do
      annotate (show err)
      failure
    Right (root, blockMap) -> do
      case fromBlockMap blockMap root of
        Left err  -> do
          annotate (show err)
          failure
        Right mst -> do
          let key = "com.example.record/3jqfcqzm3fn2j"
          case lookup key mst of
            Nothing  -> do
              annotate "key not found"
              failure
            Just cid -> cid === parsedLeafCid

-- | 'lookup' returns Nothing for a key that is not in the tree.
prop_getMissing :: Property
prop_getMissing = property $ do
  case readCarWithRoot singleEntryCar of
    Left err              -> do
      annotate (show err)
      failure
    Right (root, blockMap) ->
      case fromBlockMap blockMap root of
        Left err  -> do
          annotate (show err)
          failure
        Right mst ->
          lookup "com.example.record/does-not-exist" mst === Nothing

-- | 'diff' from empty to single-entry tree produces one WCreate.
prop_diffEmptyToSingle :: Property
prop_diffEmptyToSingle = property $ do
  case readCarWithRoot singleEntryCar of
    Left err              -> do
      annotate (show err)
      failure
    Right (root, blockMap) -> do
      case fromBlockMap blockMap root of
        Left err  -> do
          annotate (show err)
          failure
        Right mst -> do
          let writes = Tree.diff Nothing mst
          length writes === 1
          case writes of
            [WCreate k c] -> do
              k === "com.example.record/3jqfcqzm3fn2j"
              c === parsedLeafCid
            _ -> failure

-- | 'verifyProofs' succeeds when op matches tree contents.
prop_verifyProofsOk :: Property
prop_verifyProofsOk = property $ do
  case readCarWithRoot singleEntryCar of
    Left err              -> do
      annotate (show err)
      failure
    Right (root, blockMap) -> do
      case fromBlockMap blockMap root of
        Left err  -> do
          annotate (show err)
          failure
        Right mst -> do
          let op = RecordOp
                { ropCollection = "com.example.record"
                , ropRkey       = "3jqfcqzm3fn2j"
                , ropCid        = Just parsedLeafCid
                }
          case verifyProofs mst [op] of
            Left err -> do
              annotate (show err)
              failure
            Right () -> success

-- | 'verifyProofs' fails when the claimed CID is wrong.
prop_verifyProofsBadCid :: Property
prop_verifyProofsBadCid = property $ do
  case readCarWithRoot singleEntryCar of
    Left err              -> do
      annotate (show err)
      failure
    Right (root, blockMap) -> do
      case fromBlockMap blockMap root of
        Left err  -> do
          annotate (show err)
          failure
        Right mst -> do
          let wrongCid = root
              op = RecordOp
                    { ropCollection = "com.example.record"
                    , ropRkey       = "3jqfcqzm3fn2j"
                    , ropCid        = Just wrongCid
                    }
          case verifyProofs mst [op] of
            Left _  -> success
            Right _ -> do
              annotate "verification should have failed"
              failure

-- | 'fromBlockMap' after 'toBlockMap' is identity (modulo forcing all thunks).
prop_roundTripBlockMap :: Property
prop_roundTripBlockMap = property $ do
  case readCarWithRoot singleEntryCar of
    Left err              -> do
      annotate (show err)
      failure
    Right (root, blockMap) -> do
      case fromBlockMap blockMap root of
        Left err  -> do
          annotate (show err)
          failure
        Right mst -> do
          let bmap' = toBlockMap mst
          case fromBlockMap bmap' (mstCid mst) of
            Left err   -> do
              annotate (show err)
              failure
            Right mst' -> do
              -- Verify round-trip by comparing leaves (forces all thunks)
              toList mst' === toList mst
              mstCid mst' === mstCid mst

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.MST.Tree"
  [ ("lookup returns correct CID for present key",   prop_getPresent)
  , ("lookup returns Nothing for missing key",        prop_getMissing)
  , ("diff empty to single entry = 1 WCreate",        prop_diffEmptyToSingle)
  , ("verifyProofs succeeds when ops match",          prop_verifyProofsOk)
  , ("verifyProofs fails on CID mismatch",            prop_verifyProofsBadCid)
  , ("fromBlockMap . toBlockMap is identity",         prop_roundTripBlockMap)
  ]
