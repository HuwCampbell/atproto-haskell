-- | AT Protocol @com.atproto.repo.*@ XRPC method bindings.
--
-- This module provides typed Haskell wrappers for the repository XRPC
-- methods defined by the AT Protocol.  Use any 'XrpcClient' backend –
-- for example 'ATProto.XRPC.Http.HttpXrpcClient' – to make real network
-- requests.
--
-- = End-to-end usage
--
-- The typical pattern in an ATProto application is:
--
-- 1. Resolve the user's DID to find their PDS endpoint.
-- 2. Point an HTTP XRPC client at that PDS.
-- 3. Call the desired method.
--
-- @
-- import ATProto.DID
-- import ATProto.Repo
-- import ATProto.XRPC.Http
-- import qualified Data.Text as T
--
-- -- Fetch the 50 most recent posts from a user's repo.
-- fetchPosts :: T.Text -> IO ()
-- fetchPosts handle = do
--   -- Step 1: resolve the handle to a DID document.
--   plc <- defaultPlcResolver
--   did <- resolveHandle handle    -- hypothetical; use your DNS/HTTP resolver
--   docOrErr <- resolve plc did
--   case docOrErr of
--     Left  err -> print err
--     Right doc -> do
--       -- Step 2: find the PDS URL from the DID document services.
--       let pdsUrl = serviceEndpoint (head (didDocServices doc))
--
--       -- Step 3: create an XRPC client and call listRecords.
--       client  <- newHttpXrpcClient pdsUrl
--       records <- listRecords client $
--         (defaultListRecordsParams did "app.bsky.feed.post")
--           { lrpLimit = Just 20 }
--       case records of
--         Left  err  -> print err
--         Right resp -> mapM_ (print . rrUri) (lrrRecords resp)
-- @
module ATProto.Repo
  ( -- * @com.atproto.repo.listRecords@
    listRecords
  , ListRecordsParams (..)
  , defaultListRecordsParams
  , ListRecordsResponse (..)
  , RepoRecord (..)
    -- * @com.atproto.repo.createRecord@
  , createRecord
  , CreateRecordRequest (..)
  , CreateRecordResponse (..)
    -- * @com.atproto.repo.putRecord@
  , putRecord
  , PutRecordRequest (..)
  , PutRecordResponse (..)
    -- * @com.atproto.repo.getRecord@
  , getRecord
  , GetRecordParams (..)
  , defaultGetRecordParams
  , GetRecordResponse (..)
    -- * @com.atproto.repo.deleteRecord@
  , deleteRecord
  , DeleteRecordRequest (..)
  , DeleteRecordResponse (..)
    -- * @com.atproto.repo.describeRepo@
  , DescribeRepoResponse (..)
    -- * @com.atproto.repo.applyWrites@
  , ApplyWritesRequest (..)
    -- * @com.atproto.repo.uploadBlob@
  , uploadBlob
  , UploadBlobRequest (..)
  , UploadBlobResponse (..)
    -- * @com.atproto.sync.getBlob@
  , getBlob
  , GetBlobParams (..)
    -- * @com.atproto.sync.*@
  , GetRepoStatusResponse (..)
  , ListBlobsResponse (..)
  , ListReposResponse (..)
  , RepoInfo (..)
    -- * @com.atproto.server.*@
  , DescribeServerResponse (..)
  , CreateSessionRequest (..)
  , CreateSessionResponse (..)
  , GetSessionResponse (..)
    -- * @com.atproto.identity.*@
  , ResolveHandleResponse (..)
    -- * @app.bsky.actor.*@
  , GetPreferencesResponse (..)
    -- * @chat.bsky.convo.*@
  , GetLogResponse (..)
  , ListConvosResponse (..)
    -- * @app.bsky.actor.getProfile@
  , getProfile
  , GetProfileParams (..)
  , ProfileView (..)
  , CommitMeta (..)
  ) where

import ATProto.Repo.Actor
import ATProto.Repo.ApplyWrites
import ATProto.Repo.Chat
import ATProto.Repo.CommitMeta
import ATProto.Repo.CreateRecord
import ATProto.Repo.DeleteRecord
import ATProto.Repo.DescribeRepo
import ATProto.Repo.GetBlob
import ATProto.Repo.GetProfile
import ATProto.Repo.GetRecord
import ATProto.Repo.Identity
import ATProto.Repo.ListRecords
import ATProto.Repo.PutRecord
import ATProto.Repo.Server
import ATProto.Repo.Sync
import ATProto.Repo.UploadBlob
