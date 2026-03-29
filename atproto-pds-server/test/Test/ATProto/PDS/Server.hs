{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Integration tests for the PDS XRPC server.
module Test.ATProto.PDS.Server (tests) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import Hedgehog

import           Network.HTTP.Types     (status200, methodGet, methodPost, http11)
import           Network.Wai.Internal   (Request (..), RequestBodyLength (..))
import           Network.Wai.Test       (runSession, srequest, SRequest (..),
                                         simpleBody, simpleStatus)

import ATProto.Crypto                   (generateKeyPair, Curve (..))
import ATProto.PDS.Storage.InMemory     (InMemoryStore, newInMemoryStore)
import ATProto.PDS.Server

tests :: Hedgehog.Group
tests = $$discover

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkEnv :: IO (Env InMemoryStore)
mkEnv = do
  store <- newInMemoryStore
  (priv, _pub) <- generateKeyPair P256
  newEnv store priv "http://localhost:3000" "localhost"

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

prop_describeServer :: Property
prop_describeServer = withTests 1 . property $ do
  env <- evalIO mkEnv
  let app = pdsApplication env
  resp <- evalIO $ runSession
    (srequest $ SRequest
      (Network.Wai.Internal.Request
        { requestMethod = methodGet
        , rawPathInfo = "/xrpc/com.atproto.server.describeServer"
        , pathInfo = ["xrpc", "com.atproto.server.describeServer"]
        , queryString = []
        , requestHeaders = []
        , rawQueryString = ""
        , httpVersion = http11
        , isSecure = False
        , remoteHost = error "unused"
        , requestBody = return ""
        , vault = mempty
        , requestBodyLength = KnownLength 0
        , requestHeaderHost = Nothing
        , requestHeaderRange = Nothing
        , requestHeaderReferer = Nothing
        , requestHeaderUserAgent = Nothing
        })
      "")
    app

  simpleStatus resp === status200
  let body = TE.decodeUtf8Lenient (BL.toStrict (simpleBody resp))
  assert $ "availableUserDomains" `T.isInfixOf` body

prop_createAndGetSession :: Property
prop_createAndGetSession = withTests 1 . property $ do
  env <- evalIO mkEnv
  let app = pdsApplication env

  -- Create a session (auto-creates account)
  let createBody = "{\"identifier\":\"test.handle\",\"password\":\"testpass\"}"
  createResp <- evalIO $ runSession
    (srequest $ SRequest
      (Network.Wai.Internal.Request
        { requestMethod = methodPost
        , rawPathInfo = "/xrpc/com.atproto.server.createSession"
        , pathInfo = ["xrpc", "com.atproto.server.createSession"]
        , queryString = []
        , requestHeaders = [("Content-Type", "application/json")]
        , rawQueryString = ""
        , httpVersion = http11
        , isSecure = False
        , remoteHost = error "unused"
        , requestBody = return ""
        , vault = mempty
        , requestBodyLength = KnownLength (fromIntegral (BL.length createBody))
        , requestHeaderHost = Nothing
        , requestHeaderRange = Nothing
        , requestHeaderReferer = Nothing
        , requestHeaderUserAgent = Nothing
        })
      createBody)
    app

  simpleStatus createResp === status200
  let createRespBody = TE.decodeUtf8Lenient (BL.toStrict (simpleBody createResp))
  assert $ "accessJwt" `T.isInfixOf` createRespBody
  assert $ "did" `T.isInfixOf` createRespBody

  -- Extract the access token from the response
  let mToken = extractJsonField "accessJwt" createRespBody
  case mToken of
    Nothing -> do
      footnote "Failed to extract accessJwt from response"
      failure
    Just token -> do
      -- Use the token to get the session
      getResp <- evalIO $ runSession
        (srequest $ SRequest
          (Network.Wai.Internal.Request
            { requestMethod = methodGet
            , rawPathInfo = "/xrpc/com.atproto.server.getSession"
            , pathInfo = ["xrpc", "com.atproto.server.getSession"]
            , queryString = []
            , requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token))]
            , rawQueryString = ""
            , httpVersion = http11
            , isSecure = False
            , remoteHost = error "unused"
            , requestBody = return ""
            , vault = mempty
            , requestBodyLength = KnownLength 0
            , requestHeaderHost = Nothing
            , requestHeaderRange = Nothing
            , requestHeaderReferer = Nothing
            , requestHeaderUserAgent = Nothing
            })
          "")
        app

      simpleStatus getResp === status200
      let getRespBody = TE.decodeUtf8Lenient (BL.toStrict (simpleBody getResp))
      assert $ "did" `T.isInfixOf` getRespBody
      assert $ "handle" `T.isInfixOf` getRespBody

prop_resolveHandle :: Property
prop_resolveHandle = withTests 1 . property $ do
  env <- evalIO mkEnv
  let app = pdsApplication env

  -- First create a session to register the handle
  let createBody = "{\"identifier\":\"resolve.test\",\"password\":\"pass\"}"
  _ <- evalIO $ runSession
    (srequest $ SRequest
      (Network.Wai.Internal.Request
        { requestMethod = methodPost
        , rawPathInfo = "/xrpc/com.atproto.server.createSession"
        , pathInfo = ["xrpc", "com.atproto.server.createSession"]
        , queryString = []
        , requestHeaders = [("Content-Type", "application/json")]
        , rawQueryString = ""
        , httpVersion = http11
        , isSecure = False
        , remoteHost = error "unused"
        , requestBody = return ""
        , vault = mempty
        , requestBodyLength = KnownLength (fromIntegral (BL.length createBody))
        , requestHeaderHost = Nothing
        , requestHeaderRange = Nothing
        , requestHeaderReferer = Nothing
        , requestHeaderUserAgent = Nothing
        })
      createBody)
    app

  -- Now resolve the handle
  resp <- evalIO $ runSession
    (srequest $ SRequest
      (Network.Wai.Internal.Request
        { requestMethod = methodGet
        , rawPathInfo = "/xrpc/com.atproto.identity.resolveHandle"
        , pathInfo = ["xrpc", "com.atproto.identity.resolveHandle"]
        , queryString = [("handle", Just "resolve.test")]
        , requestHeaders = []
        , rawQueryString = "?handle=resolve.test"
        , httpVersion = http11
        , isSecure = False
        , remoteHost = error "unused"
        , requestBody = return ""
        , vault = mempty
        , requestBodyLength = KnownLength 0
        , requestHeaderHost = Nothing
        , requestHeaderRange = Nothing
        , requestHeaderReferer = Nothing
        , requestHeaderUserAgent = Nothing
        })
      "")
    app

  simpleStatus resp === status200
  let body = TE.decodeUtf8Lenient (BL.toStrict (simpleBody resp))
  assert $ "did" `T.isInfixOf` body

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract a JSON string field from a simple JSON object.
extractJsonField :: T.Text -> T.Text -> Maybe T.Text
extractJsonField field txt =
  let needle = "\"" <> field <> "\":"
  in case T.breakOn needle txt of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise ->
          let afterKey = T.drop (T.length needle) rest
              trimmed  = T.dropWhile (== ' ') afterKey
          in case T.uncons trimmed of
            Just ('"', remainder) ->
              Just (T.takeWhile (/= '"') remainder)
            _ -> Nothing
