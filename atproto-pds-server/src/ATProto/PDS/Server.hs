-- | AT Protocol PDS XRPC server.
--
-- This module provides 'pdsServer' and 'pdsApplication', which wire
-- together all the XRPC endpoint handlers into a WAI 'Application'
-- ready for use with Warp.
--
-- = Quick start
--
-- @
-- import ATProto.Crypto (generateKeyPair, Curve (..))
-- import ATProto.PDS.Storage.InMemory (newInMemoryStore)
-- import ATProto.PDS.Server
-- import Network.Wai.Handler.Warp (run)
--
-- main :: IO ()
-- main = do
--   store <- newInMemoryStore
--   (priv, _pub) <- generateKeyPair P256
--   env <- newEnv store priv \"https://pds.example.com\" \"pds.example.com\"
--   run 3000 (pdsApplication env)
-- @
module ATProto.PDS.Server
  ( -- * Server construction
    pdsServer
  , pdsApplication
    -- * Re-exports
  , module ATProto.PDS.Server.Env
  ) where

import           Control.Monad.Trans.Reader (runReaderT)

import           Network.Wai                (Application)

import           ATProto.PDS.Storage        (BlockStore, RepoStore,
                                            BlobStore, PreferenceStore, AccountStore)
import           ATProto.Syntax.NSID        (parseNSID)
import           ATProto.XRPC.Server        (XrpcServer, makeServer,
                                             withAuthVerifier,
                                             query, procedure)
import           ATProto.XRPC.Server.Wai    (xrpcApplication)

import           ATProto.PDS.Server.Auth
import           ATProto.PDS.Server.Env

import qualified ATProto.PDS.Server.Endpoints.Actor    as Actor
import qualified ATProto.PDS.Server.Endpoints.Chat     as Chat
import qualified ATProto.PDS.Server.Endpoints.Identity as Identity
import qualified ATProto.PDS.Server.Endpoints.Repo     as Repo
import qualified ATProto.PDS.Server.Endpoints.Server   as Server
import qualified ATProto.PDS.Server.Endpoints.Sync     as Sync

import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Server construction
-- ---------------------------------------------------------------------------

-- | Build the 'XrpcServer' with all registered PDS endpoints.
--
-- The server supports the full set of @com.atproto.server.*@,
-- @com.atproto.repo.*@, @com.atproto.sync.*@,
-- @com.atproto.identity.*@, @app.bsky.actor.*@, and
-- @chat.bsky.convo.*@ endpoints.
pdsServer
  :: (BlockStore s, RepoStore s, BlobStore s, PreferenceStore s, AccountStore s)
  => XrpcServer (AppM s) T.Text
pdsServer =
  withAuthVerifier pdsAuthVerifier $
    makeServer
      [ -- com.atproto.server
        procedure (nsid "com.atproto.server.createSession")   Server.handleCreateSession
      , query     (nsid "com.atproto.server.describeServer")  Server.handleDescribeServer
      , query     (nsid "com.atproto.server.getSession")      Server.handleGetSession
      , procedure (nsid "com.atproto.server.refreshSession")  Server.handleRefreshSession
      , procedure (nsid "com.atproto.server.activateAccount") Server.handleActivateAccount
      , procedure (nsid "com.atproto.server.deactivateAccount") Server.handleDeactivateAccount
        -- com.atproto.repo
      , procedure (nsid "com.atproto.repo.createRecord")   Repo.handleCreateRecord
      , procedure (nsid "com.atproto.repo.deleteRecord")   Repo.handleDeleteRecord
      , query     (nsid "com.atproto.repo.getRecord")      Repo.handleGetRecord
      , query     (nsid "com.atproto.repo.listRecords")    Repo.handleListRecords
      , procedure (nsid "com.atproto.repo.putRecord")       Repo.handlePutRecord
      , procedure (nsid "com.atproto.repo.applyWrites")     Repo.handleApplyWrites
      , query     (nsid "com.atproto.repo.describeRepo")   Repo.handleDescribeRepo
      , procedure (nsid "com.atproto.repo.uploadBlob")      Repo.handleUploadBlob
        -- com.atproto.sync
      , query     (nsid "com.atproto.sync.getBlob")        Sync.handleGetBlob
      , query     (nsid "com.atproto.sync.getRecord")      Sync.handleSyncGetRecord
      , query     (nsid "com.atproto.sync.getRepo")        Sync.handleGetRepo
      , query     (nsid "com.atproto.sync.getRepoStatus")  Sync.handleGetRepoStatus
      , query     (nsid "com.atproto.sync.listBlobs")      Sync.handleListBlobs
      , query     (nsid "com.atproto.sync.listRepos")      Sync.handleListRepos
      , query     (nsid "com.atproto.sync.subscribeRepos") Sync.handleSubscribeRepos
        -- com.atproto.identity
      , query     (nsid "com.atproto.identity.resolveHandle") Identity.handleResolveHandle
        -- app.bsky.actor
      , query     (nsid "app.bsky.actor.getPreferences")   Actor.handleGetPreferences
      , procedure (nsid "app.bsky.actor.putPreferences")    Actor.handlePutPreferences
        -- chat.bsky.convo
      , query     (nsid "chat.bsky.convo.getLog")          Chat.handleGetLog
      , query     (nsid "chat.bsky.convo.listConvos")      Chat.handleListConvos
      ]

-- | Build a WAI 'Application' from a server environment.
--
-- Runs the 'AppM' monad by supplying the environment via
-- 'Control.Monad.Trans.Reader.runReaderT'.
pdsApplication
  :: (BlockStore s, RepoStore s, BlobStore s, PreferenceStore s, AccountStore s)
  => Env s -> Application
pdsApplication env =
  xrpcApplication (flip runReaderT env) pdsServer

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Parse an NSID or crash at startup (all NSIDs here are compile-time
-- constants so failure indicates a programming error).
nsid :: T.Text -> ATProto.Syntax.NSID.NSID
nsid t = case parseNSID t of
  Right n -> n
  Left  _ -> error ("BUG: invalid NSID constant: " <> T.unpack t)
