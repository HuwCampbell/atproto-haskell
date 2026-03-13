-- | WebSocket client for Tap's event stream.
--
-- Connects to Tap's @\/channel@ WebSocket endpoint and dispatches parsed
-- events to caller-supplied callbacks.  Automatically reconnects with
-- exponential backoff on connection failure.
module ATProto.Tap.WebSocket
  ( TapWSConfig (..)
  , runTapWebSocket
  ) where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (SomeException, try)
import           Control.Monad              (forever, when)
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Network.WebSockets         as WS

import           ATProto.Tap.Events         (TapEvent (..), RecordEvent (..), parseTapEvent)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the Tap WebSocket client.
data TapWSConfig = TapWSConfig
  { twcHost      :: T.Text
    -- ^ Hostname, e.g. @\"localhost\"@.
  , twcPort      :: Int
    -- ^ Port, e.g. @2480@.
  , twcPath      :: T.Text
    -- ^ WebSocket path, default @\"\/channel\"@.
  , twcOnEvent   :: TapEvent -> IO ()
    -- ^ Called for each successfully parsed event.
  , twcOnError   :: String -> IO ()
    -- ^ Called on parse or connection errors.
  , twcEnableAcks :: Bool
    -- ^ Whether to send ack messages after processing each event.
  }

-- ---------------------------------------------------------------------------
-- Client
-- ---------------------------------------------------------------------------

-- | Connect to Tap and stream events indefinitely.
--
-- Reconnects automatically with exponential backoff (1s, 2s, 4s, … up to 30s)
-- when the connection drops.
runTapWebSocket :: TapWSConfig -> IO ()
runTapWebSocket cfg =
  reconnectLoop 1
  where
    host = T.unpack (twcHost cfg)
    port = twcPort cfg
    path = T.unpack (twcPath cfg)

    maxBackoff :: Int
    maxBackoff = 30

    reconnectLoop :: Int -> IO ()
    reconnectLoop backoff = do
      result <- try (WS.runClient host port path (app cfg))
                  :: IO (Either SomeException ())
      case result of
        Left ex -> do
          twcOnError cfg ("WebSocket connection error: " <> show ex)
          threadDelay (backoff * 1000000)
          reconnectLoop (min (backoff * 2) maxBackoff)
        Right _ -> do
          -- Clean disconnect; reconnect immediately
          reconnectLoop 1

app :: TapWSConfig -> WS.ClientApp ()
app cfg conn = forever $ do
  msg <- WS.receiveData conn :: IO LBS.ByteString
  case Aeson.eitherDecode msg of
    Left err ->
      twcOnError cfg ("JSON decode error: " <> err)
    Right val ->
      case parseTapEvent val of
        Left err ->
          twcOnError cfg ("Event parse error: " <> err)
        Right evt -> do
          twcOnEvent cfg evt
          when (twcEnableAcks cfg) $
            sendAck conn evt

sendAck :: WS.Connection -> TapEvent -> IO ()
sendAck conn evt =
  case eventId evt of
    Nothing  -> pure ()
    Just eid ->
      let ack = Aeson.object
                  [ "type" Aeson..= ("ack" :: T.Text)
                  , "id"   Aeson..= eid
                  ]
      in WS.sendTextData conn (Aeson.encode ack)

eventId :: TapEvent -> Maybe Int
eventId (TapRecordEvent re) = Just (reId re)
eventId _                   = Nothing
