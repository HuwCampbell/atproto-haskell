-- | Typed bindings for @com.atproto.sync.*@ XRPC methods.
module ATProto.Repo.Sync
  ( -- * @com.atproto.sync.getRepoStatus@
    GetRepoStatusResponse (..)
  , getRepoStatusResponseCodec
    -- * @com.atproto.sync.listBlobs@
  , ListBlobsResponse (..)
  , listBlobsResponseCodec
    -- * @com.atproto.sync.listRepos@
  , ListReposResponse (..)
  , listReposResponseCodec
  , RepoInfo (..)
  , repoInfoCodec
  ) where

import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- ---------------------------------------------------------------------------
-- com.atproto.sync.getRepoStatus
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.sync.getRepoStatus@.
data GetRepoStatusResponse = GetRepoStatusResponse
  { grsrDid    :: T.Text
  , grsrActive :: Bool
  , grsrStatus :: Maybe T.Text
  , grsrRev    :: Maybe T.Text
  } deriving (Eq, Show)

getRepoStatusResponseCodec :: Codec GetRepoStatusResponse
getRepoStatusResponseCodec =
    Codec.record "com.atproto.sync.getRepoStatus#output" $
        GetRepoStatusResponse
            <$> Codec.requiredField "did"    Codec.did  grsrDid
            <*> Codec.requiredField "active" Codec.bool grsrActive
            <*> Codec.optionalField "status" Codec.text grsrStatus
            <*> Codec.optionalField "rev"    Codec.text grsrRev

-- ---------------------------------------------------------------------------
-- com.atproto.sync.listBlobs
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.sync.listBlobs@.
data ListBlobsResponse = ListBlobsResponse
  { lbrCids   :: [T.Text]
  , lbrCursor :: Maybe T.Text
  } deriving (Eq, Show)

listBlobsResponseCodec :: Codec ListBlobsResponse
listBlobsResponseCodec =
    Codec.record "com.atproto.sync.listBlobs#output" $
        ListBlobsResponse
            <$> Codec.requiredField "cids"   (Codec.array Codec.text) lbrCids
            <*> Codec.optionalField "cursor" Codec.text               lbrCursor

-- ---------------------------------------------------------------------------
-- com.atproto.sync.listRepos
-- ---------------------------------------------------------------------------

-- | A single repository entry in the @listRepos@ response.
data RepoInfo = RepoInfo
  { riDid    :: T.Text
  , riActive :: Bool
  , riStatus :: Maybe T.Text
  } deriving (Eq, Show)

repoInfoCodec :: Codec RepoInfo
repoInfoCodec =
    Codec.record "com.atproto.sync.listRepos#repo" $
        RepoInfo
            <$> Codec.requiredField "did"    Codec.did  riDid
            <*> Codec.requiredField "active" Codec.bool riActive
            <*> Codec.optionalField "status" Codec.text riStatus

-- | Response from @com.atproto.sync.listRepos@.
data ListReposResponse = ListReposResponse
  { lrrRepos  :: [RepoInfo]
  , lrrCursor :: Maybe T.Text
  } deriving (Eq, Show)

listReposResponseCodec :: Codec ListReposResponse
listReposResponseCodec =
    Codec.record "com.atproto.sync.listRepos#output" $
        ListReposResponse
            <$> Codec.requiredField "repos"  (Codec.array repoInfoCodec) lrrRepos
            <*> Codec.optionalField "cursor" Codec.text                  lrrCursor
