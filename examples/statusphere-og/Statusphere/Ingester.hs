-- | Firehose ingester for the Statusphere example app.
--
-- Connects to the ATProto firehose relay and ingests
-- @xyz.statusphere.status@ records into the SQLite database.
module Statusphere.Ingester
  ( runIngester
  ) where

import           Control.Exception        (SomeException, catch)
import           Control.Monad            (when)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import qualified Data.Map                 as Map
import           Data.Foldable            (traverse_)
import           Data.Time.Clock          (getCurrentTime)
import           Data.Time.Format         (formatTime, defaultTimeLocale)
import           Database.SQLite.Simple   (Connection)

import           ATProto.DID              (DidResolver (..), defaultDidResolver)
import qualified ATProto.Lex.Cbor         as Cbor
import           ATProto.Car              (readCar, cidToText)

import           ATProto.Firehose.Client
import           ATProto.Firehose.Events  (CommitEvent (..), RepoOp (..), OpAction (..))

import           Statusphere.Database     (StatusRow (..), upsertStatus,
                                           deleteStatus)
import           Statusphere.Types        (StatusView (svStatus), statusViewCodec)


-- | The collection NSID for statusphere status records.
statusCollection :: T.Text
statusCollection = "xyz.statusphere.status"

-- | Start the firehose ingester in the current thread.
--
-- This function blocks forever, automatically reconnecting on errors.
runIngester :: Connection -> IO ()
runIngester conn = do
  resolver <- defaultDidResolver
  let
    fhc =
      FirehoseConfig
        { fcRelay   = "bsky.network"
        , fcCursor  = Nothing
        , fcOnEvent = \_ -> return ()
        , fcOnError = handleError
        }
    afc =
      AuthFirehoseConfig {
        afcBase = fhc
      , afcResolveDid = \t -> do
          res <- resolve resolver t
          pure $ case res of
            Left x -> Left (show x)
            Right a -> Right a
      , afcOnUnverifiable = \_ _ -> return ()
      , afcOnVerified = mapM_ . handleOp conn
      }

  runAuthFirehose afc

-- | Handle a single record operation.
handleOp :: Connection -> CommitEvent -> RepoOp -> IO ()
handleOp conn ce op = do
  let did = ceRepo ce
      (collection, rkey) = splitPath (ropPath op)
      runInsert =
        traverse_ (ingestStatus conn did collection rkey) $ do
          (_,m)  <- hush $ readCar (ceBlocks ce)
          let tm  = Map.mapKeys cidToText m
          cid    <- ropCid op
          found  <- Map.lookup cid tm
          hush $ Cbor.deserialise statusViewCodec (BL.fromStrict found)

  when (collection == statusCollection) $
    case ropAction op of
      OpCreate -> runInsert
      OpUpdate -> runInsert
      OpDelete -> do
        let uri = "at://" <> did <> "/" <> collection <> "/" <> rkey
        safeIO $ deleteStatus conn uri

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

-- | Ingest a create/update operation for a status record.
--
-- We extract the status emoji from the rkey-based URI path. The firehose
-- doesn't directly give us the record content in the basic (unverified)
-- mode, so we store a placeholder and let the record get filled in from
-- the actual firehose blocks in the authenticated mode.
--
-- For the basic firehose we just record that a status was set.
ingestStatus :: Connection -> T.Text -> T.Text -> T.Text -> StatusView -> IO ()
ingestStatus conn did collection rkey status = do
  now <- isoNow
  let uri = "at://" <> did <> "/" <> collection <> "/" <> rkey
  -- Insert into the status into the database.
  safeIO $ upsertStatus conn StatusRow
    { srUri       = uri
    , srAuthorDid = did
    , srStatus    = svStatus status  -- placeholder (firehose basic mode)
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
isoNow =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000Z" <$> getCurrentTime

-- | Catch and ignore any IO exception.
safeIO :: IO () -> IO ()
safeIO action = action `catch` handler
  where
    handler :: SomeException -> IO ()
    handler _ = return ()
