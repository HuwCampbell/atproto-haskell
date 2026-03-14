-- | Event types for Tap's WebSocket stream.
--
-- Tap delivers JSON events over its @\/channel@ WebSocket endpoint.
-- Each event carries an @id@ field and a @type@ tag that determines the
-- payload shape.
module ATProto.Tap.Events
  ( TapEvent (..)
  , RecordEvent (..)
  , RecordAction (..)
  , IdentityEvent (..)
  , parseTapEvent
  ) where

import qualified Data.Aeson        as Aeson
import qualified Data.Aeson.Types  as Aeson
import qualified Data.Text         as T

-- ---------------------------------------------------------------------------
-- Record action
-- ---------------------------------------------------------------------------

-- | The kind of mutation a record event describes.
data RecordAction
  = RaCreate
  | RaUpdate
  | RaDelete
  deriving (Show, Eq)

parseRecordAction :: T.Text -> Either String RecordAction
parseRecordAction "create" = Right RaCreate
parseRecordAction "update" = Right RaUpdate
parseRecordAction "delete" = Right RaDelete
parseRecordAction other    = Left ("Unknown record action: " <> T.unpack other)

-- ---------------------------------------------------------------------------
-- Record event
-- ---------------------------------------------------------------------------

-- | A record-level event (create, update, or delete).
data RecordEvent = RecordEvent
  { reId         :: Int
  , reLive       :: Bool
  , reRev        :: T.Text
  , reDid        :: T.Text
  , reCollection :: T.Text
  , reRkey       :: T.Text
  , reAction     :: RecordAction
  , reCid        :: Maybe T.Text
  , reRecord     :: Maybe Aeson.Value
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Identity event
-- ---------------------------------------------------------------------------

-- | An identity event (handle or status change).
data IdentityEvent = IdentityEvent
  { ieDid      :: T.Text
  , ieHandle   :: T.Text
  , ieIsActive :: Bool
  , ieStatus   :: T.Text
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Top-level event
-- ---------------------------------------------------------------------------

-- | A tagged union of all Tap event types.
data TapEvent
  = TapRecordEvent   RecordEvent
  | TapIdentityEvent IdentityEvent
  | TapUnknown       T.Text
  deriving (Show, Eq)

-- | Parse a JSON 'Aeson.Value' into a 'TapEvent'.
parseTapEvent :: Aeson.Value -> Either String TapEvent
parseTapEvent = Aeson.parseEither parseTapEvent'

parseTapEvent' :: Aeson.Value -> Aeson.Parser TapEvent
parseTapEvent' = Aeson.withObject "TapEvent" $ \o -> do
  typ <- o Aeson..: "type" :: Aeson.Parser T.Text
  case typ of
    "record"   -> TapRecordEvent   <$> parseRecordEvent o
    "identity" -> TapIdentityEvent <$> parseIdentityEvent o
    other      -> pure (TapUnknown other)

parseRecordEvent :: Aeson.Object -> Aeson.Parser RecordEvent
parseRecordEvent o = do
  eid <- o Aeson..: "id"
  rec' <- o Aeson..: "record"
  Aeson.withObject "RecordPayload" (\r -> do
    live       <- r Aeson..: "live"
    rev        <- r Aeson..: "rev"
    did'       <- r Aeson..: "did"
    collection <- r Aeson..: "collection"
    rkey       <- r Aeson..: "rkey"
    actionTxt  <- r Aeson..: "action" :: Aeson.Parser T.Text
    action     <- case parseRecordAction actionTxt of
                    Left err -> fail err
                    Right a  -> pure a
    cid'       <- r Aeson..:? "cid"
    record     <- r Aeson..:? "record"
    pure RecordEvent
      { reId         = eid
      , reLive       = live
      , reRev        = rev
      , reDid        = did'
      , reCollection = collection
      , reRkey       = rkey
      , reAction     = action
      , reCid        = cid'
      , reRecord     = record
      }) rec'

parseIdentityEvent :: Aeson.Object -> Aeson.Parser IdentityEvent
parseIdentityEvent o = do
  ident <- o Aeson..: "identity"
  Aeson.withObject "IdentityPayload" (\i -> do
    did'     <- i Aeson..: "did"
    handle   <- i Aeson..: "handle"
    isActive <- i Aeson..: "is_active"
    status   <- i Aeson..: "status"
    pure IdentityEvent
      { ieDid      = did'
      , ieHandle   = handle
      , ieIsActive = isActive
      , ieStatus   = status
      }) ident
