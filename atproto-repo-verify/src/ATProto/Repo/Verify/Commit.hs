-- | Commit CBOR decoding and signature verification.
--
-- The signing payload is the DAG-CBOR encoding of the commit map *without*
-- the @sig@ field.  Fields are ordered canonically: by byte-length of key
-- first, then lexicographically.  For a v3 commit the unsigned fields in
-- canonical order are:
--
-- @did@ (3), @rev@ (3), @data@ (4), @prev@ (4), @version@ (7)
-- Within length-3: @did@ \< @rev@ (d \< r).
-- Within length-4: @data@ \< @prev@ (d \< p).
module ATProto.Repo.Verify.Commit
  ( -- * Decoding
    decodeCommit
    -- * Signature verification
  , verifyCommitSig
    -- * Full pipeline
  , verifyCommitCar
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Codec.CBOR.Decoding  as D
import qualified Codec.CBOR.Read      as R
import qualified Codec.CBOR.Encoding  as E
import qualified Codec.CBOR.Write     as W

import ATProto.Car.Cid            (CidBytes)
import ATProto.Car.BlockMap       (BlockMap)
import ATProto.Car.DagCbor        (encodeCidTag42, encodeNullableCidTag42,
                                   decodeCidTag42, decodeNullableCidTag42,
                                   skipValue)
import ATProto.Car.Parser         (readCarWithRoot, CarError (..))
import ATProto.Crypto.Types       (PubKey, Signature (..), SigStrictness (..))
import ATProto.Crypto.EC          (verify)
import ATProto.DID.Document       (DidDocument)
import ATProto.MST.Diff           (mstDiff, WriteDescr)
import ATProto.MST.Verify         (RecordOp (..), verifyProofs)
import ATProto.MST.Types          (MstError (..))
import ATProto.Repo.Verify.Types  (Commit (..), VerifyError (..))
import ATProto.Repo.Verify.Key    (resolveAtprotoKey)

-- ---------------------------------------------------------------------------
-- Commit decoding
-- ---------------------------------------------------------------------------

-- | Decode a commit block from the block map.
decodeCommit :: BlockMap -> CidBytes -> Either VerifyError Commit
decodeCommit bmap cid =
  case Map.lookup cid bmap of
    Nothing  -> Left (VerifyCommitDecodeError "commit block not found in block map")
    Just raw ->
      case R.deserialiseFromBytes commitDecoder (BL.fromStrict raw) of
        Left  err      -> Left (VerifyCommitDecodeError (show err))
        Right (_, c)   -> Right c

-- | CBOR decoder for the commit map.
commitDecoder :: D.Decoder s Commit
commitDecoder = do
  n <- D.decodeMapLen
  parseCommitMap n Nothing Nothing Nothing Nothing Nothing Nothing

parseCommitMap
  :: Int
  -> Maybe T.Text          -- did
  -> Maybe Int             -- version
  -> Maybe T.Text          -- rev
  -> Maybe (Maybe CidBytes) -- prev
  -> Maybe CidBytes        -- data
  -> Maybe BS.ByteString   -- sig
  -> D.Decoder s Commit
parseCommitMap 0 mDid mVer mRev mPrev mData mSig = do
  did     <- requireField "did"     mDid
  ver     <- requireField "version" mVer
  rev     <- requireField "rev"     mRev
  dat     <- requireField "data"    mData
  sig     <- requireField "sig"     mSig
  let prev = case mPrev of { Nothing -> Nothing; Just mp -> mp }
  return (Commit did ver rev prev dat sig)
  where
    requireField name Nothing  = fail ("commit missing field: " ++ name)
    requireField _    (Just v) = return v
parseCommitMap n mDid mVer mRev mPrev mData mSig = do
  key <- D.decodeString
  case key of
    "did" -> do
      v <- D.decodeString
      parseCommitMap (n-1) (Just v) mVer mRev mPrev mData mSig
    "version" -> do
      v <- D.decodeWord
      parseCommitMap (n-1) mDid (Just (fromIntegral v)) mRev mPrev mData mSig
    "rev" -> do
      v <- D.decodeString
      parseCommitMap (n-1) mDid mVer (Just v) mPrev mData mSig
    "prev" -> do
      cid <- decodeNullableCidTag42
      parseCommitMap (n-1) mDid mVer mRev (Just cid) mData mSig
    "data" -> do
      cid <- decodeCidTag42
      parseCommitMap (n-1) mDid mVer mRev mPrev (Just cid) mSig
    "sig" -> do
      v <- D.decodeBytes
      parseCommitMap (n-1) mDid mVer mRev mPrev mData (Just v)
    _ -> do
      -- Skip unknown field: try common types
      skipValue
      parseCommitMap (n-1) mDid mVer mRev mPrev mData mSig

-- ---------------------------------------------------------------------------
-- Canonical DAG-CBOR re-encoding (unsigned commit)
-- ---------------------------------------------------------------------------

-- | Re-encode the commit *without* the @sig@ field as canonical DAG-CBOR.
--
-- Field order (canonical = length then lexicographic):
-- @did@ (3), @rev@ (3), @data@ (4), @prev@ (4), @version@ (7)
encodeUnsignedCommit :: Commit -> BS.ByteString
encodeUnsignedCommit c =
  BL.toStrict . W.toLazyByteString $ encoding
  where
    encoding =
      -- Map of 5 or 4 fields depending on whether prev is present.
      -- Always include all 5 fields; encode prev as null if Nothing.
      E.encodeMapLen 5
      -- "did" (len 3, lex order 1st)
      <> E.encodeString "did"
      <> E.encodeString (commitDid c)
      -- "rev" (len 3, lex order 2nd)
      <> E.encodeString "rev"
      <> E.encodeString (commitRev c)
      -- "data" (len 4, lex order 1st)
      <> E.encodeString "data"
      <> encodeCidTag42 (commitData c)
      -- "prev" (len 4, lex order 2nd)
      <> E.encodeString "prev"
      <> encodeNullableCidTag42 (commitPrev c)
      -- "version" (len 7)
      <> E.encodeString "version"
      <> E.encodeInt (commitVersion c)

-- ---------------------------------------------------------------------------
-- Signature verification
-- ---------------------------------------------------------------------------

-- | Verify the commit signature against the given public key.
verifyCommitSig :: Commit -> PubKey -> Either VerifyError ()
verifyCommitSig c pubKey =
  let payload = encodeUnsignedCommit c
      sig     = Signature (commitSig c)
  in if verify Strict pubKey payload sig
     then Right ()
     else Left VerifyBadSig

-- ---------------------------------------------------------------------------
-- Full pipeline
-- ---------------------------------------------------------------------------

-- | Verify a commit CAR (the @blocks@ field from a firehose @#commit@ event).
--
-- Steps:
--
-- 1. Parse the CAR file.
-- 2. Decode the commit block from the root CID.
-- 3. Assert @commit.did == expectedDid@.
-- 4. Extract the @#atproto@ signing key from the DID document.
-- 5. Verify the commit signature.
-- 6. Verify MST proofs against @commit.data@.
-- 7. Compute and return the write list via 'mstDiff'.
verifyCommitCar
  :: DidDocument
  -> T.Text          -- ^ expected DID (from firehose event)
  -> BS.ByteString   -- ^ raw CAR bytes (evt.blocks)
  -> [RecordOp]      -- ^ ops asserted by the event
  -> Either VerifyError [WriteDescr]
verifyCommitCar doc expectedDid carBytes ops = do
  -- 1. Parse CAR
  (rootCid, bmap) <- mapCarError (readCarWithRoot carBytes)
  -- 2. Decode commit
  commit          <- decodeCommit bmap rootCid
  -- 3. DID check
  if commitDid commit /= expectedDid then
    Left (VerifyDidMismatch (commitDid commit) expectedDid)
  else do
    -- 4. Key extraction
    pubKey <- resolveAtprotoKey doc
    -- 5. Signature verification
    verifyCommitSig commit pubKey
    -- 6. MST proof verification
    mapMstError (verifyProofs bmap (commitData commit) ops)
    -- 7. Tree diff
    mapMstError (mstDiff bmap Nothing (commitData commit))
  where
    mapCarError :: Either CarError a -> Either VerifyError a
    mapCarError (Left err)  = Left (VerifyCarError err)
    mapCarError (Right x)   = Right x

    mapMstError :: Either MstError a -> Either VerifyError a
    mapMstError (Left err)  = Left (VerifyMstError err)
    mapMstError (Right x)   = Right x
