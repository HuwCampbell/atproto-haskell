-- | Event ADTs for the AT Protocol firehose.
--
-- These types correspond to the five event types emitted by
-- @com.atproto.sync.subscribeRepos@.
module ATProto.Firehose.Events
  ( -- * Operations
    OpAction (..)
  , RepoOp (..)
    -- * Events
  , CommitEvent (..)
  , IdentityEvent (..)
  , AccountEvent (..)
  , SyncEvent (..)
  , InfoEvent (..)
  , FirehoseEvent (..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T

-- | The action taken on a single record.
data OpAction
  = OpCreate  -- ^ Record created.
  | OpUpdate  -- ^ Record updated.
  | OpDelete  -- ^ Record deleted.
  deriving (Eq, Show)

-- | A single record operation within a @#commit@ event.
data RepoOp = RepoOp
  { ropAction :: OpAction
    -- ^ The type of operation.
  , ropPath   :: T.Text
    -- ^ @\"collection\/rkey\"@ path of the record.
  , ropCid    :: Maybe T.Text
    -- ^ The new CID of the record, as multibase text.
    -- 'Nothing' for deletes.
  } deriving (Show)

-- | A @#commit@ event: a new commit to a repository.
data CommitEvent = CommitEvent
  { ceSeq    :: Int
    -- ^ Sequence number (monotonically increasing per relay).
  , ceRepo   :: T.Text
    -- ^ DID of the repository owner.
  , ceCommit :: T.Text
    -- ^ CID of the commit (as multibase text).
  , ceRev    :: T.Text
    -- ^ TID of the commit revision.
  , ceSince  :: Maybe T.Text
    -- ^ TID of the previous commit known to the relay.
  , ceBlocks :: BS.ByteString
    -- ^ Raw CAR v1 bytes containing the commit and changed blocks.
  , ceOps    :: [RepoOp]
    -- ^ The record operations in this commit.
  , ceTooBig :: Bool
    -- ^ True if the blocks field is incomplete due to size limits.
  , ceTime   :: T.Text
    -- ^ ISO-8601 timestamp string.
  } deriving (Show)

-- | An @#identity@ event: a DID's handle or DID document changed.
data IdentityEvent = IdentityEvent
  { ieSeq    :: Int
  , ieDid    :: T.Text
  , ieHandle :: Maybe T.Text
  , ieTime   :: T.Text
  } deriving (Show)

-- | An @#account@ event: a repository's account status changed.
data AccountEvent = AccountEvent
  { aeSeq    :: Int
  , aeDid    :: T.Text
  , aeActive :: Bool
    -- ^ Whether the account is currently active.
  , aeStatus :: Maybe T.Text
    -- ^ Reason for inactivity: @\"takendown\"@, @\"suspended\"@,
    -- @\"deleted\"@, or @\"deactivated\"@.
  , aeTime   :: T.Text
  } deriving (Show)

-- | A @#sync@ event: a full repository CAR.
data SyncEvent = SyncEvent
  { seSeq    :: Int
  , seDid    :: T.Text
  , seRev    :: T.Text
  , seBlocks :: BS.ByteString
    -- ^ Raw CAR v1 bytes of the full repository.
  , seTime   :: T.Text
  } deriving (Show)

-- | An @#info@ event: an informational message from the relay.
data InfoEvent = InfoEvent
  { infoName    :: T.Text
  , infoMessage :: Maybe T.Text
  } deriving (Show)

-- | A decoded firehose event.
data FirehoseEvent
  = FECommit   CommitEvent
  | FEIdentity IdentityEvent
  | FEAccount  AccountEvent
  | FESync     SyncEvent
  | FEInfo     InfoEvent
  | FEUnknown  T.Text
    -- ^ An event type not recognised by this decoder (forward-compat).
  deriving (Show)
