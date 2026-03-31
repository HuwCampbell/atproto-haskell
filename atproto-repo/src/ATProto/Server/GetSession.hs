-- | Typed binding for @com.atproto.server.getSession@.
--
-- Get information about the current auth session.  Requires auth.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/server/getSession.json>.
module ATProto.Server.GetSession
  ( -- * Response
    GetSessionResponse (..)
    -- * Codecs
  , getSessionResponseCodec
    -- * Client function
  , getSession
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.server.getSession@.
data GetSessionResponse = GetSessionResponse
  { gsresHandle          :: T.Text
    -- ^ The account handle.
  , gsresDid             :: T.Text
    -- ^ The account DID.
  , gsresDidDoc          :: Maybe LexValue
    -- ^ The complete DID document.
  , gsresEmail           :: Maybe T.Text
    -- ^ The account email.
  , gsresEmailConfirmed  :: Maybe Bool
    -- ^ Whether the email is confirmed.
  , gsresEmailAuthFactor :: Maybe Bool
    -- ^ Whether email auth factor is enabled.
  , gsresActive          :: Maybe Bool
    -- ^ Whether the account is active.
  , gsresStatus          :: Maybe T.Text
    -- ^ If active=false, possible reason (takendown, suspended, deactivated).
  } deriving (Eq, Show)

-- | Codec for the @getSession@ response body.
getSessionResponseCodec :: Codec GetSessionResponse
getSessionResponseCodec =
    Codec.record "com.atproto.server.getSession#response" $
        GetSessionResponse
            <$> Codec.requiredField "handle"          Codec.handle    gsresHandle
            <*> Codec.requiredField "did"             Codec.did       gsresDid
            <*> Codec.optionalField "didDoc"          Codec.lexValue  gsresDidDoc
            <*> Codec.optionalField "email"           Codec.text      gsresEmail
            <*> Codec.optionalField "emailConfirmed"  Codec.bool      gsresEmailConfirmed
            <*> Codec.optionalField "emailAuthFactor" Codec.bool      gsresEmailAuthFactor
            <*> Codec.optionalField "active"          Codec.bool      gsresActive
            <*> Codec.optionalField "status"          Codec.text      gsresStatus

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.server.getSession@ using the given XRPC client.
--
-- Requires auth; the access JWT should be included in the request headers
-- by the 'XrpcClient' backend.
getSession
  :: XrpcClient c
  => c
  -> IO (Either XrpcError GetSessionResponse)
getSession client = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.server.getSession"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError GetSessionResponse
parseResponse body =
  case LexJson.decode getSessionResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
