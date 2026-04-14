-- | WebSocket firehose client.
--
-- Connects to the AT Protocol relay's @subscribeRepos@ WebSocket endpoint,
-- decodes incoming DAG-CBOR frames, and dispatches events to caller-supplied
-- callbacks.  The client automatically reconnects using the last seen
-- sequence number as a cursor when the connection drops.
module ATProto.Firehose.Client
  ( -- * Configuration
    FirehoseConfig (..)
  , FirehoseClientError (..)
    -- * Authenticated client
  , AuthFirehoseConfig (..)
    -- * Running
  , runFirehose
  , runAuthFirehose
  ) where

import           Control.Exception        (SomeException, try)
import           Control.Monad            (forever)
import           Data.IORef
import qualified Data.ByteString          as BS
import qualified Data.Text                as T
import qualified Network.WebSockets       as WS
import qualified Wuss

import ATProto.DID.Document              (DidDocument)
import qualified ATProto.MST.Verify      as MV
import ATProto.Repo.Verify               (verifyCommitCar)
import ATProto.Firehose.Events
import ATProto.Firehose.Frame            (decodeFrame, FrameError (..))
import ATProto.Car                       (textToCidBytes)

-- ---------------------------------------------------------------------------
-- Configuration types
-- ---------------------------------------------------------------------------

-- | Configuration for a basic (unauthenticated) firehose client.
data FirehoseConfig = FirehoseConfig
  { fcRelay    :: T.Text
    -- ^ Relay hostname, e.g. @\"bsky.network\"@.
  , fcCursor   :: Maybe Int
    -- ^ Optional starting cursor (sequence number).
  , fcOnEvent  :: FirehoseEvent -> IO ()
    -- ^ Callback invoked for each decoded event.
  , fcOnError  :: FirehoseClientError -> IO ()
    -- ^ Callback invoked on frame decode errors.
  }

-- | Errors that can occur in the firehose client.
data FirehoseClientError
  = FceFrameError FrameError
    -- ^ A WebSocket frame could not be decoded.
  | FceWebSocketError String
    -- ^ A WebSocket-level error occurred.
  | FceVerifyError T.Text T.Text
    -- ^ Commit verification failed: @(did, error description)@.
  deriving (Show)

-- | Configuration for an authenticated firehose client that verifies commit
-- signatures and MST proofs.
data AuthFirehoseConfig = AuthFirehoseConfig
  { afcBase           :: FirehoseConfig
    -- ^ Underlying unauthenticated client config.
  , afcResolveDid     :: T.Text -> IO (Either String DidDocument)
    -- ^ Resolve a DID string to a DID document.  Called for each commit event.
    -- Should implement key-rotation retry (call twice on failure).
  , afcOnVerified     :: CommitEvent -> [RepoOp] -> IO ()
    -- ^ Invoked with the verified event and its write list on success.
  , afcOnUnverifiable :: CommitEvent -> String -> IO ()
    -- ^ Invoked when the commit cannot be verified (e.g. @tooBig = True@).
  }

-- ---------------------------------------------------------------------------
-- Basic client
-- ---------------------------------------------------------------------------

-- | Connect to the relay and stream events indefinitely.
--
-- Reconnects automatically with the last seen cursor on disconnect.
runFirehose :: FirehoseConfig -> IO ()
runFirehose cfg = do
  cursorRef <- newIORef (fcRelay cfg `seq` fcCursor cfg)
  forever (runOnce cursorRef)
  where
    host = T.unpack (fcRelay cfg)

    runOnce :: IORef (Maybe Int) -> IO ()
    runOnce cursorRef = do
      mCursor <- readIORef cursorRef
      let path = "/xrpc/com.atproto.sync.subscribeRepos" ++
                 case mCursor of
                   Nothing -> ""
                   Just n  -> "?cursor=" ++ show n
      result <- try (Wuss.runSecureClient host 443 path (app cursorRef)) :: IO (Either SomeException ())
      case result of
        Left ex -> fcOnError cfg (FceWebSocketError (show ex))
        Right _ -> return ()

    app :: IORef (Maybe Int) -> WS.ClientApp ()
    app cursorRef conn = forever $ do
      msg <- WS.receiveData conn :: IO BS.ByteString
      case decodeFrame msg of
        Left err  -> fcOnError cfg (FceFrameError err)
        Right evt -> do
          -- Update the cursor from commit/identity/account/sync sequence numbers
          case seqOf evt of
            Just n  -> writeIORef cursorRef (Just n)
            Nothing -> return ()
          fcOnEvent cfg evt

seqOf :: FirehoseEvent -> Maybe Int
seqOf (FECommit   e) = Just (ceSeq e)
seqOf (FEIdentity e) = Just (ieSeq e)
seqOf (FEAccount  e) = Just (aeSeq e)
seqOf (FESync     e) = Just (seSeq e)
seqOf _              = Nothing

-- ---------------------------------------------------------------------------
-- Authenticated client
-- ---------------------------------------------------------------------------

-- | Connect to the relay, verify each commit, and dispatch verified events.
runAuthFirehose :: AuthFirehoseConfig -> IO ()
runAuthFirehose authCfg =
  runFirehose (afcBase authCfg) { fcOnEvent = handleEvent }
  where
    handleEvent (FECommit ce)
      | ceTooBig ce =
          afcOnUnverifiable authCfg ce "commit is too large (tooBig=True)"
      | otherwise = do
          let did = ceRepo ce
          -- First attempt at DID resolution
          mDoc1 <- afcResolveDid authCfg did
          case mDoc1 of
            Left err -> do
              -- Resolution failed entirely
              afcOnUnverifiable authCfg ce ("DID resolution failed: " ++ err)
            Right doc1 -> do
              case traverse toRecordOp (ceOps ce) of
                Left err1 ->
                  afcOnUnverifiable authCfg ce ("Parsing CID failed: " ++ err1)
                Right ops ->
                  case verifyCommitCar doc1 did (ceBlocks ce) ops of
                    Right () ->
                      afcOnVerified authCfg ce (ceOps ce)
                    Left err ->
                      afcOnUnverifiable authCfg ce (show err)
    handleEvent evt =
      -- Pass non-commit events through unchanged
      fcOnEvent (afcBase authCfg) evt

toRecordOp :: RepoOp -> Either String MV.RecordOp
toRecordOp (RepoOp _ path cidText _) = do
  mCid  <- traverse textToCidBytes cidText
  return
    MV.RecordOp
      { MV.ropKey        = path
      , MV.ropCid        = mCid
      }
