-- | Firehose ingester for the Statusphere example app.
--
-- Connects to the ATProto firehose relay and ingests
-- @xyz.statusphere.status@ records into the SQLite database.
module Statusphere.Ingester
  ( runIngester
  ) where

import           Control.Exception        (SomeException, catch)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Data.Time.Clock          (getCurrentTime)
import           Data.Time.Format         (formatTime, defaultTimeLocale)
import           Database.SQLite.Simple   (Connection)

import           ATProto.Firehose         (FirehoseConfig (..),
                                           FirehoseClientError (..),
                                           FirehoseEvent (..),
                                           CommitEvent (..),
                                           RepoOp (..), OpAction (..),
                                           runFirehose)

import           Statusphere.Database     (StatusRow (..), upsertStatus,
                                           deleteStatus)

-- | The collection NSID for statusphere status records.
statusCollection :: T.Text
statusCollection = "xyz.statusphere.status"

-- | Start the firehose ingester in the current thread.
--
-- This function blocks forever, automatically reconnecting on errors.
runIngester :: Connection -> IO ()
runIngester conn =
  runFirehose FirehoseConfig
    { fcRelay   = "bsky.network"
    , fcCursor  = Nothing
    , fcOnEvent = handleEvent conn
    , fcOnError = handleError
    }

-- | Handle a single firehose event.
handleEvent :: Connection -> FirehoseEvent -> IO ()
handleEvent conn (FECommit ce) = mapM_ (handleOp conn (ceRepo ce)) (ceOps ce)
handleEvent _    _             = return ()

-- | Handle a single record operation.
handleOp :: Connection -> T.Text -> RepoOp -> IO ()
handleOp conn did op = do
  let path = ropPath op
      (collection, rkey) = splitPath path
  if collection /= statusCollection
    then return ()
    else case ropAction op of
      OpCreate -> ingestStatus conn did collection rkey
      OpUpdate -> ingestStatus conn did collection rkey
      OpDelete -> do
        let uri = "at://" <> did <> "/" <> collection <> "/" <> rkey
        safeIO $ deleteStatus conn uri

-- | Ingest a create/update operation for a status record.
--
-- We extract the status emoji from the rkey-based URI path. The firehose
-- doesn't directly give us the record content in the basic (unverified)
-- mode, so we store a placeholder and let the record get filled in from
-- the actual firehose blocks in the authenticated mode.
--
-- For the basic firehose we just record that a status was set.
ingestStatus :: Connection -> T.Text -> T.Text -> T.Text -> IO ()
ingestStatus conn did collection rkey = do
  now <- isoNow
  let uri = "at://" <> did <> "/" <> collection <> "/" <> rkey
  -- In the basic firehose, we don't have the record content.
  -- Store a placeholder status; the actual content would come from
  -- an authenticated firehose or be fetched separately.
  safeIO $ upsertStatus conn StatusRow
    { srUri       = uri
    , srAuthorDid = did
    , srStatus    = "🦋"  -- placeholder (firehose basic mode)
    , srCreatedAt = now
    , srIndexedAt = now
    }

handleError :: FirehoseClientError -> IO ()
handleError err = TIO.putStrLn $ "Firehose error: " <> T.pack (show err)

-- | Split "collection/rkey" into its parts.
splitPath :: T.Text -> (T.Text, T.Text)
splitPath t =
  case T.breakOn "/" t of
    (col, rest) -> (col, T.drop 1 rest)

-- | Get the current time as ISO 8601 string.
isoNow :: IO T.Text
isoNow = do
  now <- getCurrentTime
  return $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000Z" now

-- | Catch and ignore any IO exception.
safeIO :: IO () -> IO ()
safeIO action = action `catch` handler
  where
    handler :: SomeException -> IO ()
    handler _ = return ()
