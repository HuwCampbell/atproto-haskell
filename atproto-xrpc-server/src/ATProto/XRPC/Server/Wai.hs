{-# LANGUAGE RankNTypes #-}
-- | WAI integration for the XRPC server framework.
--
-- Provides 'xrpcMiddleware' and 'xrpcApplication' for wiring an
-- 'XrpcServer' into any WAI-compatible server such as Warp.
module ATProto.XRPC.Server.Wai
  ( xrpcMiddleware
  , xrpcApplication
  ) where

import           Control.Monad.IO.Class  (MonadIO)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.CaseInsensitive    as CI
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE

import           Network.HTTP.Types.Header  (hContentType)
import           Network.HTTP.Types.Method  (methodGet, methodPost)
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI     (queryToQueryText)
import           Network.Wai

import           ATProto.Syntax.NSID        (NSID, nsidSegments, parseNSID)
import           ATProto.XRPC.Types         (XrpcHeaders, XrpcMethod (..))
import           ATProto.XRPC.Server.Types
-- | Build a WAI 'Middleware' from an 'XrpcServer'.
--
-- Intercepts requests whose path matches @\/xrpc\/<nsid>@, dispatches to
-- the matching handler, and serialises the result.  Requests that do not
-- match the @\/xrpc\/@ prefix are passed unchanged to the next application.
--
-- HTTP routing rules:
--
-- * @GET \/xrpc\/<nsid>@ dispatches to a registered query handler.
-- * @POST \/xrpc\/<nsid>@ dispatches to a registered procedure handler.
-- * An invalid NSID in the path returns HTTP 400 @InvalidNSID@.
-- * A valid NSID with the wrong HTTP method returns HTTP 405 @MethodNotAllowed@.
-- * A valid NSID with no registered handler returns HTTP 501 @MethodNotImplemented@.
-- * Any other HTTP method on an @\/xrpc\/@ path returns HTTP 405.
xrpcMiddleware
  :: MonadIO m
  => (forall a. m a -> IO a)
  -- ^ Runner: how to execute the application monad in 'IO'.
  -- Supplied once at the WAI boundary, e.g. @'flip' 'Control.Monad.Trans.Reader.runReaderT' env@.
  -> XrpcServer m did
  -> Middleware
xrpcMiddleware runner server nextApp req respond =
  case pathInfo req of
    ["xrpc", nsidText] ->
      case parseNSID nsidText of
        Left _ ->
          respond $ errorResponse status400 "InvalidNSID"
            (Just ("Invalid NSID: " <> nsidText))
        Right nsid ->
          let httpMethod = requestMethod req
          in if httpMethod /= methodGet && httpMethod /= methodPost
             then respond $ errorResponse status405 "MethodNotAllowed"
                    (Just "XRPC endpoints only accept GET (query) or POST (procedure)")
             else
               let xrpcMethod = if httpMethod == methodGet
                                 then XrpcQuery
                                 else XrpcProcedure
               in case Map.lookup (xrpcMethod, nsid) (xsEndpoints server) of
                    Just endpoint -> do
                      let params = buildParams req
                      body <- if xrpcMethod == XrpcProcedure
                              then fmap Just (strictRequestBody req)
                              else return Nothing
                      let hdrs = buildHeaders req
                      -- Run auth verifier (if configured) before calling the handler.
                      authResult <- case xsAuthVerifier server of
                        Nothing -> return (AuthOk Nothing)
                        Just v  -> runner (v hdrs)
                      case authResult of
                        AuthFailed code msg ->
                          respond (unauthorizedResponse code msg)
                        AuthOk caller -> do
                          let xreq = XrpcServerRequest nsid params body hdrs caller
                          result <- runner (xeHandler endpoint xreq)
                          respond (resultToResponse result)
                    Nothing ->
                      let otherMethod = case xrpcMethod of
                            XrpcQuery     -> XrpcProcedure
                            XrpcProcedure -> XrpcQuery
                      in if Map.member (otherMethod, nsid) (xsEndpoints server)
                         then respond $ errorResponse status405 "MethodNotAllowed"
                                (Just ( "This NSID is registered as a "
                                     <> methodLabel otherMethod
                                     <> "; use "
                                     <> methodHttpVerb otherMethod ))
                         else respond $ errorResponse status501 "MethodNotImplemented"
                                (Just ("NSID not implemented: " <> renderNsid nsid))
    _ -> nextApp req respond

-- | Convenience: standalone WAI 'Application' that returns HTTP 404 for
-- any path that is not a recognised @\/xrpc\/<nsid>@ path.
xrpcApplication
  :: MonadIO m
  => (forall a. m a -> IO a)
  -> XrpcServer m did
  -> Application
xrpcApplication runner server =
  xrpcMiddleware runner server notFoundApp
  where
    notFoundApp _req respond =
      respond $ errorResponse status404 "NotFound" (Just "Not found")

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Build an XRPC error 'Response'.
errorResponse :: Status -> T.Text -> Maybe T.Text -> Response
errorResponse st code msg =
  responseLBS st [(hContentType, "application/json")] (buildErrorJson code msg)

-- | Build an HTTP 401 Unauthorized 'Response' for auth failures.
unauthorizedResponse :: T.Text -> Maybe T.Text -> Response
unauthorizedResponse = errorResponse status401

-- | Serialise an 'XrpcHandlerResult' to a WAI 'Response'.
resultToResponse :: XrpcHandlerResult -> Response
resultToResponse (XrpcSuccess body) =
  responseLBS status200 [(hContentType, "application/json")] body
resultToResponse XrpcAccepted =
  responseLBS status202 [] BL.empty
resultToResponse (XrpcHandlerError code msg) =
  errorResponse status400 code msg

-- | Decode WAI query parameters into a plain 'Map'.
buildParams :: Request -> Map.Map T.Text T.Text
buildParams req = Map.fromList
  [ (k, v)
  | (k, Just v) <- queryToQueryText (queryString req)
  ]

-- | Convert WAI request headers into 'XrpcHeaders'.
buildHeaders :: Request -> XrpcHeaders
buildHeaders req = Map.fromList
  [ (CI.map TE.decodeUtf8 k, TE.decodeUtf8 v)
  | (k, v) <- requestHeaders req
  ]

-- | Reconstruct the canonical dot-separated text form of an NSID.
renderNsid :: NSID -> T.Text
renderNsid = T.intercalate "." . nsidSegments

-- | Human-readable label for an 'XrpcMethod'.
methodLabel :: XrpcMethod -> T.Text
methodLabel XrpcQuery     = "query"
methodLabel XrpcProcedure = "procedure"

-- | HTTP verb string for an 'XrpcMethod'.
methodHttpVerb :: XrpcMethod -> T.Text
methodHttpVerb XrpcQuery     = "GET"
methodHttpVerb XrpcProcedure = "POST"

-- ---------------------------------------------------------------------------
-- Minimal JSON serialisation (no aeson dependency)
-- ---------------------------------------------------------------------------

-- | Serialise an XRPC error object to lazy 'BL.ByteString' JSON.
buildErrorJson :: T.Text -> Maybe T.Text -> BL.ByteString
buildErrorJson code Nothing =
  "{\"error\":" <> jsonString code <> "}"
buildErrorJson code (Just msg) =
  "{\"error\":" <> jsonString code <> ",\"message\":" <> jsonString msg <> "}"

-- | Encode a 'T.Text' value as a JSON string literal.
jsonString :: T.Text -> BL.ByteString
jsonString t =
  BL.fromStrict (TE.encodeUtf8 ("\"" <> T.concatMap escapeChar t <> "\""))
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = T.singleton c
