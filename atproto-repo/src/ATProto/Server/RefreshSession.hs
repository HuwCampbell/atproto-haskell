-- | Typed binding for @com.atproto.server.refreshSession@.
--
-- Refresh an authentication session.  Requires auth using the
-- @refreshJwt@ (not the @accessJwt@).
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/server/refreshSession.json>.
module ATProto.Server.RefreshSession
  ( -- * Response
    RefreshSessionResponse (..)
    -- * Codecs
  , refreshSessionResponseCodec
    -- * Client function
  , refreshSession
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

-- | Response from @com.atproto.server.refreshSession@.
data RefreshSessionResponse = RefreshSessionResponse
  { rsresAccessJwt       :: T.Text
    -- ^ New short-lived access token.
  , rsresRefreshJwt      :: T.Text
    -- ^ New longer-lived refresh token.
  , rsresHandle          :: T.Text
    -- ^ The account handle.
  , rsresDid             :: T.Text
    -- ^ The account DID.
  , rsresDidDoc          :: Maybe LexValue
    -- ^ The complete DID document.
  , rsresEmail           :: Maybe T.Text
    -- ^ The account email.
  , rsresEmailConfirmed  :: Maybe Bool
    -- ^ Whether the email is confirmed.
  , rsresEmailAuthFactor :: Maybe Bool
    -- ^ Whether email auth factor is enabled.
  , rsresActive          :: Maybe Bool
    -- ^ Whether the account is active.
  , rsresStatus          :: Maybe T.Text
    -- ^ Hosting status of the account.  If not specified, assume @\"active\"@.
  } deriving (Eq, Show)

-- | Codec for the @refreshSession@ response body.
refreshSessionResponseCodec :: Codec RefreshSessionResponse
refreshSessionResponseCodec =
    Codec.record "com.atproto.server.refreshSession#response" $
        RefreshSessionResponse
            <$> Codec.requiredField "accessJwt"       Codec.text      rsresAccessJwt
            <*> Codec.requiredField "refreshJwt"      Codec.text      rsresRefreshJwt
            <*> Codec.requiredField "handle"          Codec.handle    rsresHandle
            <*> Codec.requiredField "did"             Codec.did       rsresDid
            <*> Codec.optionalField "didDoc"          Codec.lexValue  rsresDidDoc
            <*> Codec.optionalField "email"           Codec.text      rsresEmail
            <*> Codec.optionalField "emailConfirmed"  Codec.bool      rsresEmailConfirmed
            <*> Codec.optionalField "emailAuthFactor" Codec.bool      rsresEmailAuthFactor
            <*> Codec.optionalField "active"          Codec.bool      rsresActive
            <*> Codec.optionalField "status"          Codec.text      rsresStatus

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.server.refreshSession@ using the given XRPC client.
--
-- Requires auth using the refresh JWT.
refreshSession
  :: XrpcClient c
  => c
  -> IO (Either XrpcError RefreshSessionResponse)
refreshSession client = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.server.refreshSession"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError RefreshSessionResponse
parseResponse body =
  case LexJson.decode refreshSessionResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
