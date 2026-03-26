-- | Types shared across the repo-verify package.
module ATProto.Repo.Verify.Types
  ( -- * Commit
    Commit (..)
    -- * Errors
  , VerifyError (..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import ATProto.Car.Cid (CidBytes)

-- | A decoded AT Protocol repository commit.
data Commit = Commit
  { commitDid     :: T.Text
    -- ^ The DID of the repository owner.
  , commitVersion :: Int
    -- ^ Commit schema version (2 = legacy, 3 = current).
  , commitRev     :: T.Text
    -- ^ TID string identifying this revision.
  , commitPrev    :: Maybe CidBytes
    -- ^ CID of the previous commit, or Nothing for the first commit.
  , commitData    :: CidBytes
    -- ^ CID of the MST root for this commit.
  , commitSig     :: BS.ByteString
    -- ^ Raw compact ECDSA signature bytes (64 bytes, r||s).
  } deriving (Show)

-- | Errors that can occur during commit or record verification.
data VerifyError
  = VerifyNoAtprotoKey
    -- ^ The DID document has no @#atproto@ verification method.
  | VerifyBadKey String
    -- ^ The @#atproto@ key could not be decoded.
  | VerifyBadSig
    -- ^ The commit signature does not match the signing key.
  | VerifyDidMismatch T.Text T.Text
    -- ^ The commit's DID field does not match the expected DID.
  | VerifyMstError T.Text
    -- ^ An MST-layer error occurred.
  | VerifyCarError T.Text
    -- ^ A CAR-layer error occurred.
  | VerifyCommitDecodeError String
    -- ^ CBOR decoding of the commit block failed.
  deriving (Eq, Show)
