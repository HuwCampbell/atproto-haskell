-- | HTTP\/TLS backend for the AT Protocol XRPC client.
--
-- 'HttpXrpcClient' is a concrete implementation of the 'XrpcClient' typeclass
-- backed by @http-client-tls@.  It maps 'XrpcRequest' values to HTTP requests
-- and converts HTTP responses to 'XrpcResponse' or 'XrpcError'.
--
-- = Typical usage
--
-- @
-- import ATProto.XRPC
-- import ATProto.XRPC.Http
-- import qualified Data.Map.Strict as Map
--
-- main :: IO ()
-- main = do
--   client <- newHttpXrpcClient "https://bsky.social"
--
--   result <- xrpcQuery client "app.bsky.feed.getTimeline"
--               (Map.fromList [("limit", "20")])
--   case result of
--     Left  err  -> print err
--     Right resp -> print (xrpcRespStatus resp)
-- @
--
-- == Attaching a bearer token
--
-- @
--   let authed = XrpcRequest
--         { xrpcReqMethod  = XrpcQuery
--         , xrpcReqNsid    = "com.atproto.repo.listRecords"
--         , xrpcReqParams  = Map.fromList [("repo", did), ("collection", col)]
--         , xrpcReqBody    = Nothing
--         , xrpcReqHeaders = Map.fromList [("Authorization", "Bearer " <> jwt)]
--         }
--   result <- runXrpc client authed
-- @
module ATProto.XRPC.Http
  ( -- * Client
    HttpXrpcClient
  , newHttpXrpcClient
  , newHttpXrpcClientWith
  ) where

import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import qualified Data.CaseInsensitive    as CI
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Network.HTTP.Client     (Manager, Request, RequestBody (..),
                                          httpLbs, method, newManager,
                                          parseRequest, queryString,
                                          requestBody, requestHeaders,
                                          responseBody, responseHeaders,
                                          responseStatus)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types      (renderQuery, toQuery, statusCode)

import ATProto.XRPC.Client (XrpcClient (..))
import ATProto.XRPC.Types  (XrpcError (..), XrpcMethod (..), XrpcRequest (..),
                             XrpcResponse (..))

-- | An XRPC client backed by @http-client-tls@.
data HttpXrpcClient = HttpXrpcClient
  { httpXrpcPdsUrl  :: T.Text
    -- ^ Base URL of the PDS, e.g. @\"https://bsky.social\"@.
  , httpXrpcManager :: Manager
    -- ^ Shared TLS-capable connection manager.
  }

-- | Create an 'HttpXrpcClient' with a fresh TLS-capable connection manager.
newHttpXrpcClient :: T.Text -> IO HttpXrpcClient
newHttpXrpcClient pdsUrl = do
  mgr <- newManager tlsManagerSettings
  return (HttpXrpcClient pdsUrl mgr)

-- | Create an 'HttpXrpcClient' sharing an existing 'Manager'.
--
-- Useful when the same connection pool serves both DID resolution and XRPC.
newHttpXrpcClientWith :: Manager -> T.Text -> HttpXrpcClient
newHttpXrpcClientWith mgr pdsUrl = HttpXrpcClient pdsUrl mgr

instance XrpcClient HttpXrpcClient where
  runXrpc = runHttpXrpc

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

runHttpXrpc
  :: HttpXrpcClient
  -> XrpcRequest
  -> IO (Either XrpcError XrpcResponse)
runHttpXrpc client xrpcReq = do
  req  <- buildRequest client xrpcReq
  resp <- httpLbs req (httpXrpcManager client)
  let status = statusCode (responseStatus resp)
      body   = responseBody resp
      hdrs   = Map.fromList
                 [ (TE.decodeUtf8Lenient (CI.original k), TE.decodeUtf8Lenient v)
                 | (k, v) <- responseHeaders resp
                 ]
  if status >= 200 && status < 300
    then return $ Right XrpcResponse
           { xrpcRespStatus  = status
           , xrpcRespHeaders = hdrs
           , xrpcRespBody    = body
           }
    else return $ Left (parseXrpcError status hdrs body)

-- | Construct an HTTP 'Request' from an XRPC request descriptor.
buildRequest :: HttpXrpcClient -> XrpcRequest -> IO Request
buildRequest client xrpcReq = do
  let url  = T.unpack (httpXrpcPdsUrl client)
           ++ "/xrpc/"
           ++ T.unpack (xrpcReqNsid xrpcReq)
      meth = case xrpcReqMethod xrpcReq of
               XrpcQuery     -> BC.pack "GET"
               XrpcProcedure -> BC.pack "POST"
      qs   = renderQuery True
               (toQuery [ (TE.encodeUtf8 k, TE.encodeUtf8 v)
                        | (k, v) <- Map.toList (xrpcReqParams xrpcReq)
                        ])
      body = case xrpcReqBody xrpcReq of
               Nothing -> RequestBodyLBS BL.empty
               Just b  -> RequestBodyLBS b
      hdrs = [ (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v)
             | (k, v) <- Map.toList (xrpcReqHeaders xrpcReq)
             ]
  base <- parseRequest url
  return base
    { method         = meth
    , queryString    = qs
    , requestBody    = body
    , requestHeaders = hdrs ++ requestHeaders base
    }

-- | Parse an XRPC error from an HTTP error-response body.
--
-- The AT Protocol mandates @{ \"error\": \"...\", \"message\": \"...\" }@.
-- Falls back to a generic error token when the body is not valid JSON.
parseXrpcError :: Int -> Map.Map T.Text T.Text -> BL.ByteString -> XrpcError
parseXrpcError status hdrs body =
  case (Aeson.eitherDecode body :: Either String (Map.Map T.Text Aeson.Value)) of
    Right m ->
      XrpcError
        { xrpcErrError   = lookupStr "error"   m
        , xrpcErrMessage = lookupMaybeStr "message" m
        , xrpcErrStatus  = status
        , xrpcErrHeaders = hdrs
        }
    Left _ ->
      XrpcError
        { xrpcErrError   = "Error"
        , xrpcErrMessage = Nothing
        , xrpcErrStatus  = status
        , xrpcErrHeaders = hdrs
        }
  where
    lookupStr k m =
      case Map.lookup k m of
        Just (Aeson.String t) -> t
        _                     -> "Error"
    lookupMaybeStr k m =
      case Map.lookup k m of
        Just (Aeson.String t) -> Just t
        _                     -> Nothing
