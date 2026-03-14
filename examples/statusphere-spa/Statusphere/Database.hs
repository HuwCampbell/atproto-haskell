-- | SQLite database for the Statusphere SPA example.
--
-- Manages the @status@ table for ingested statuses.
module Statusphere.Database
  ( -- * Types
    StatusRow (..)
    -- * Connection management
  , withDatabase
    -- * Migrations
  , migrate
    -- * Status queries
  , getRecentStatuses
  , getMyStatus
  , insertStatus
  , upsertStatus
  , deleteStatus
  ) where

import qualified Data.Text               as T
import           Database.SQLite.Simple

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A row from the @status@ table.
data StatusRow = StatusRow
  { srUri       :: T.Text
  , srAuthorDid :: T.Text
  , srStatus    :: T.Text
  , srCreatedAt :: T.Text
  , srIndexedAt :: T.Text
  } deriving (Eq, Show)

instance FromRow StatusRow where
  fromRow = StatusRow <$> field <*> field <*> field <*> field <*> field

instance ToRow StatusRow where
  toRow s = toRow (srUri s, srAuthorDid s, srStatus s, srCreatedAt s, srIndexedAt s)

-- ---------------------------------------------------------------------------
-- Connection management
-- ---------------------------------------------------------------------------

-- | Open a connection to the SQLite database, run the action, and close.
withDatabase :: FilePath -> (Connection -> IO a) -> IO a
withDatabase path action = withConnection path $ \conn -> do
  execute_ conn "PRAGMA journal_mode=WAL"
  execute_ conn "PRAGMA busy_timeout=5000"
  action conn

-- ---------------------------------------------------------------------------
-- Migrations
-- ---------------------------------------------------------------------------

-- | Run database migrations (creates tables if they don't exist).
migrate :: Connection -> IO ()
migrate conn =
  execute_ conn
    "CREATE TABLE IF NOT EXISTS status (\
    \  uri       TEXT PRIMARY KEY, \
    \  authorDid TEXT NOT NULL, \
    \  status    TEXT NOT NULL, \
    \  createdAt TEXT NOT NULL, \
    \  indexedAt TEXT NOT NULL \
    \)"

-- ---------------------------------------------------------------------------
-- Status queries
-- ---------------------------------------------------------------------------

-- | Fetch the 50 most recent statuses.
getRecentStatuses :: Connection -> IO [StatusRow]
getRecentStatuses conn =
  query_ conn
    "SELECT uri, authorDid, status, createdAt, indexedAt \
    \FROM status ORDER BY indexedAt DESC LIMIT 50"

-- | Get the most recent status for a specific user.
getMyStatus :: Connection -> T.Text -> IO (Maybe StatusRow)
getMyStatus conn did' = do
  rows <- query conn
    "SELECT uri, authorDid, status, createdAt, indexedAt \
    \FROM status WHERE authorDid = ? \
    \ORDER BY indexedAt DESC LIMIT 1"
    (Only did')
  case rows of
    (r:_) -> return (Just r)
    []    -> return Nothing

-- | Insert a status (optimistic write after putRecord).
insertStatus :: Connection -> StatusRow -> IO ()
insertStatus conn s =
  execute conn
    "INSERT OR IGNORE INTO status (uri, authorDid, status, createdAt, indexedAt) \
    \VALUES (?, ?, ?, ?, ?)"
    s

-- | Upsert a status (used by the Tap ingester).
upsertStatus :: Connection -> StatusRow -> IO ()
upsertStatus conn s =
  execute conn
    "INSERT INTO status (uri, authorDid, status, createdAt, indexedAt) \
    \VALUES (?, ?, ?, ?, ?) \
    \ON CONFLICT(uri) DO UPDATE SET status = excluded.status, indexedAt = excluded.indexedAt"
    s

-- | Delete a status by its AT-URI.
deleteStatus :: Connection -> T.Text -> IO ()
deleteStatus conn uri' =
  execute conn "DELETE FROM status WHERE uri = ?" (Only uri')
