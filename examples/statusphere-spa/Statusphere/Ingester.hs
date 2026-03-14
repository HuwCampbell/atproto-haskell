-- | Tap-based ingester for the Statusphere SPA example.
--
-- Connects to a Tap instance's WebSocket channel and ingests
-- @xyz.statusphere.status@ records into the SQLite database.
--
-- Unlike the statusphere-og example which uses the raw firehose,
-- this version uses Tap for synchronisation.  Tap delivers verified
-- record events as JSON with the full record body included, so we
-- can decode the status using the Lex codec directly.
module Statusphere.Ingester
  ( runIngester
  ) where

import           Control.Exception        (SomeException, catch)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Data.Time.Clock          (getCurrentTime)
import           Data.Time.Format         (formatTime, defaultTimeLocale)
import           Database.SQLite.Simple   (Connection)

import qualified ATProto.Lex.Json         as LexJson
import           ATProto.Tap              (TapWSConfig (..),
                                           TapEvent (..),
                                           RecordEvent (..),
                                           RecordAction (..),
                                           runTapWebSocket)

import           Statusphere.Database     (StatusRow (..), upsertStatus,
                                           deleteStatus)
import           Statusphere.Types        (StatusView (..), statusViewCodec)

-- | The collection NSID for statusphere status records.
statusCollection :: T.Text
statusCollection = "xyz.statusphere.status"

-- | Start the Tap-based ingester in the current thread.
--
-- Connects to Tap's WebSocket channel and processes record events.
-- This function blocks forever, automatically reconnecting on errors.
runIngester :: Connection -> T.Text -> Int -> IO ()
runIngester conn tapHost tapPort =
  runTapWebSocket TapWSConfig
    { twcHost       = tapHost
    , twcPort       = tapPort
    , twcPath       = "/channel"
    , twcOnEvent    = handleEvent conn
    , twcOnError    = handleError
    , twcEnableAcks = True
    }

-- | Handle a single Tap event.
handleEvent :: Connection -> TapEvent -> IO ()
handleEvent conn (TapRecordEvent re)
  | reCollection re == statusCollection = handleRecordEvent conn re
handleEvent _ _ = return ()

-- | Handle a record event for the statusphere collection.
handleRecordEvent :: Connection -> RecordEvent -> IO ()
handleRecordEvent conn re =
  case reAction re of
    RaCreate -> upsertFromEvent conn re
    RaUpdate -> upsertFromEvent conn re
    RaDelete -> do
      let uri = "at://" <> reDid re <> "/" <> reCollection re <> "/" <> reRkey re
      safeIO $ deleteStatus conn uri

-- | Upsert a status from a Tap record event.
--
-- Tap delivers the full record body in the event, so we decode it
-- using the Lex codec to extract the status emoji.
upsertFromEvent :: Connection -> RecordEvent -> IO ()
upsertFromEvent conn re =
  case reRecord re of
    Nothing -> return ()  -- No record body (shouldn't happen for create/update)
    Just val ->
      case LexJson.fromJSON statusViewCodec val of
        Left _err -> return ()  -- Skip records we can't decode
        Right sv  -> do
          now <- isoNow
          let uri = "at://" <> reDid re <> "/" <> reCollection re <> "/" <> reRkey re
          safeIO $ upsertStatus conn StatusRow
            { srUri       = uri
            , srAuthorDid = reDid re
            , srStatus    = svStatus sv
            , srCreatedAt = svCreatedAt sv
            , srIndexedAt = now
            }

handleError :: String -> IO ()
handleError err = TIO.putStrLn $ "Tap error: " <> T.pack err

-- | Get the current time as ISO 8601 string.
isoNow :: IO T.Text
isoNow =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000Z" <$> getCurrentTime

-- | Catch and ignore any IO exception.
safeIO :: IO () -> IO ()
safeIO action = action `catch` handler
  where
    handler :: SomeException -> IO ()
    handler _ = return ()
