-- | MST proof verification.
--
-- 'verifyProofs' checks that each 'RecordOp' is consistent with the MST
-- rooted at the given CID.  A 'RecordOp' asserts either that a key maps to
-- a particular CID (create/update), or that the key is absent (delete).
module ATProto.MST.Verify
  ( -- * Record operation
    RecordOp (..)
    -- * Verification
  , verifyProofs
  ) where

import qualified Data.Text as T

import ATProto.Car.Cid      (CidBytes)
import ATProto.Car.BlockMap (BlockMap)
import ATProto.MST.Get      (get)
import ATProto.MST.Types    (MstError (..))

-- | An assertion about a single record in the repository.
data RecordOp = RecordOp
  { ropCollection :: T.Text
    -- ^ The Lexicon collection NSID, e.g. @\"app.bsky.feed.post\"@.
  , ropRkey       :: T.Text
    -- ^ The record key within the collection.
  , ropCid        :: Maybe CidBytes
    -- ^ Expected CID.  'Nothing' asserts the record is absent.
  }

-- | Verify that all 'RecordOp's are consistent with the MST.
--
-- Returns @Right ()@ if every assertion holds, or @Left err@ on the first
-- mismatch or lookup error.
verifyProofs
  :: BlockMap
  -> CidBytes     -- ^ MST root CID
  -> [RecordOp]
  -> Either MstError ()
verifyProofs bmap mstRoot ops = mapM_ checkOp ops
  where
    checkOp (RecordOp col rkey expectedCid) = do
      let key = col <> "/" <> rkey
      foundCid <- get bmap mstRoot key
      case (foundCid, expectedCid) of
        (Nothing, Nothing)   -> Right ()  -- both absent
        (Just c,  Just e)
          | c == e           -> Right ()  -- present with matching CID
          | otherwise        -> Left (MstDecodeError
              (T.pack ("CID mismatch for key " ++ T.unpack key)))
        (Nothing, Just _)    -> Left (MstDecodeError
              (T.pack ("expected key present but absent: " ++ T.unpack key)))
        (Just _,  Nothing)   -> Left (MstDecodeError
              (T.pack ("expected key absent but present: " ++ T.unpack key)))
