{-# LANGUAGE OverloadedStrings #-}
module Test.ATProto.Tap.Events (tests) where

import           Hedgehog
import qualified Data.Aeson       as Aeson
import qualified Data.Text        as T

import           ATProto.Tap.Events

tests :: Group
tests = Group "ATProto.Tap.Events"
  [ ("prop_parseRecordCreate",   prop_parseRecordCreate)
  , ("prop_parseRecordDelete",   prop_parseRecordDelete)
  , ("prop_parseIdentityEvent",  prop_parseIdentityEvent)
  , ("prop_parseUnknownType",    prop_parseUnknownType)
  , ("prop_parseMissingType",    prop_parseMissingType)
  ]

-- | Parse a create-record event with all fields.
prop_parseRecordCreate :: Property
prop_parseRecordCreate = withTests 1 . property $ do
  let json = Aeson.object
        [ "id"   Aeson..= (12345 :: Int)
        , "type" Aeson..= ("record" :: T.Text)
        , "record" Aeson..= Aeson.object
            [ "live"       Aeson..= True
            , "rev"        Aeson..= ("3kb3fge5lm32x" :: T.Text)
            , "did"        Aeson..= ("did:plc:abc123" :: T.Text)
            , "collection" Aeson..= ("app.bsky.feed.post" :: T.Text)
            , "rkey"       Aeson..= ("3kb3fge5lm32x" :: T.Text)
            , "action"     Aeson..= ("create" :: T.Text)
            , "cid"        Aeson..= ("bafyreig" :: T.Text)
            , "record"     Aeson..= Aeson.object ["text" Aeson..= ("hello" :: T.Text)]
            ]
        ]
  case parseTapEvent json of
    Left err -> do
      annotate err
      failure
    Right (TapRecordEvent re) -> do
      reId re         === 12345
      reLive re       === True
      reRev re        === "3kb3fge5lm32x"
      reDid re        === "did:plc:abc123"
      reCollection re === "app.bsky.feed.post"
      reRkey re       === "3kb3fge5lm32x"
      reAction re     === RaCreate
      reCid re        === Just "bafyreig"
    Right other -> do
      annotate ("Expected TapRecordEvent, got: " <> show other)
      failure

-- | Parse a delete-record event with optional fields absent.
prop_parseRecordDelete :: Property
prop_parseRecordDelete = withTests 1 . property $ do
  let json = Aeson.object
        [ "id"   Aeson..= (99 :: Int)
        , "type" Aeson..= ("record" :: T.Text)
        , "record" Aeson..= Aeson.object
            [ "live"       Aeson..= False
            , "rev"        Aeson..= ("rev1" :: T.Text)
            , "did"        Aeson..= ("did:plc:xyz" :: T.Text)
            , "collection" Aeson..= ("app.bsky.graph.follow" :: T.Text)
            , "rkey"       Aeson..= ("rk1" :: T.Text)
            , "action"     Aeson..= ("delete" :: T.Text)
            ]
        ]
  case parseTapEvent json of
    Left err -> do
      annotate err
      failure
    Right (TapRecordEvent re) -> do
      reAction re === RaDelete
      reCid re    === Nothing
      reRecord re === Nothing
    Right other -> do
      annotate ("Expected TapRecordEvent, got: " <> show other)
      failure

-- | Parse an identity event.
prop_parseIdentityEvent :: Property
prop_parseIdentityEvent = withTests 1 . property $ do
  let json = Aeson.object
        [ "id"   Aeson..= (12346 :: Int)
        , "type" Aeson..= ("identity" :: T.Text)
        , "identity" Aeson..= Aeson.object
            [ "did"      Aeson..= ("did:plc:abc123" :: T.Text)
            , "handle"   Aeson..= ("alice.bsky.social" :: T.Text)
            , "isActive" Aeson..= True
            , "status"   Aeson..= ("active" :: T.Text)
            ]
        ]
  case parseTapEvent json of
    Left err -> do
      annotate err
      failure
    Right (TapIdentityEvent ie) -> do
      ieDid ie      === "did:plc:abc123"
      ieHandle ie   === "alice.bsky.social"
      ieIsActive ie === True
      ieStatus ie   === "active"
    Right other -> do
      annotate ("Expected TapIdentityEvent, got: " <> show other)
      failure

-- | Unknown types produce TapUnknown.
prop_parseUnknownType :: Property
prop_parseUnknownType = withTests 1 . property $ do
  let json = Aeson.object
        [ "id"   Aeson..= (1 :: Int)
        , "type" Aeson..= ("future_event" :: T.Text)
        ]
  case parseTapEvent json of
    Left err -> do
      annotate err
      failure
    Right (TapUnknown t) ->
      t === "future_event"
    Right other -> do
      annotate ("Expected TapUnknown, got: " <> show other)
      failure

-- | Missing type field produces an error.
prop_parseMissingType :: Property
prop_parseMissingType = withTests 1 . property $ do
  let json = Aeson.object [ "id" Aeson..= (1 :: Int) ]
  case parseTapEvent json of
    Left _  -> success
    Right _ -> failure
