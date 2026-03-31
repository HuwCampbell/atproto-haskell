-- | Typed binding for @com.atproto.server.createSession@.
--
-- Create an authentication session.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/server/createSession.json>.
module ATProto.Server.CreateSession
  ( -- * Request
    CreateSessionRequest (..)
    -- * Response
  , CreateSessionResponse (..)
    -- * Codecs
  , createSessionRequestCodec
  , createSessionResponseCodec
    -- * Client function
  , createSession
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
-- Request
-- ---------------------------------------------------------------------------

-- | Input body for @com.atproto.server.createSession@.
data CreateSessionRequest = CreateSessionRequest
  { csreqIdentifier     :: T.Text
    -- ^ Handle or other identifier supported by the server for the
    -- authenticating user.
  , csreqPassword       :: T.Text
    -- ^ Account password.
  , csreqAuthFactorToken :: Maybe T.Text
    -- ^ Optional auth factor token (e.g. email 2FA).
  , csreqAllowTakendown  :: Maybe Bool
    -- ^ When true, return a narrow-scoped token instead of erroring for
    -- takendown accounts.
  } deriving (Eq, Show)

-- | Codec for the @createSession@ request body.
createSessionRequestCodec :: Codec CreateSessionRequest
createSessionRequestCodec =
    Codec.record "com.atproto.server.createSession#request" $
        CreateSessionRequest
            <$> Codec.requiredField "identifier"      Codec.text  csreqIdentifier
            <*> Codec.requiredField "password"        Codec.text  csreqPassword
            <*> Codec.optionalField "authFactorToken" Codec.text  csreqAuthFactorToken
            <*> Codec.optionalField "allowTakendown"  Codec.bool  csreqAllowTakendown

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.server.createSession@.
data CreateSessionResponse = CreateSessionResponse
  { csresAccessJwt      :: T.Text
    -- ^ Short-lived access token.
  , csresRefreshJwt     :: T.Text
    -- ^ Longer-lived refresh token.
  , csresHandle         :: T.Text
    -- ^ The account handle.
  , csresDid            :: T.Text
    -- ^ The account DID.
  , csresDidDoc         :: Maybe LexValue
    -- ^ The complete DID document.
  , csresEmail          :: Maybe T.Text
    -- ^ The account email.
  , csresEmailConfirmed :: Maybe Bool
    -- ^ Whether the email is confirmed.
  , csresEmailAuthFactor :: Maybe Bool
    -- ^ Whether email auth factor is enabled.
  , csresActive         :: Maybe Bool
    -- ^ Whether the account is active.
  , csresStatus         :: Maybe T.Text
    -- ^ If active=false, possible reason (takendown, suspended, deactivated).
  } deriving (Eq, Show)

-- | Codec for the @createSession@ response body.
createSessionResponseCodec :: Codec CreateSessionResponse
createSessionResponseCodec =
    Codec.record "com.atproto.server.createSession#response" $
        CreateSessionResponse
            <$> Codec.requiredField "accessJwt"       Codec.text      csresAccessJwt
            <*> Codec.requiredField "refreshJwt"      Codec.text      csresRefreshJwt
            <*> Codec.requiredField "handle"          Codec.handle    csresHandle
            <*> Codec.requiredField "did"             Codec.did       csresDid
            <*> Codec.optionalField "didDoc"          Codec.lexValue  csresDidDoc
            <*> Codec.optionalField "email"           Codec.text      csresEmail
            <*> Codec.optionalField "emailConfirmed"  Codec.bool      csresEmailConfirmed
            <*> Codec.optionalField "emailAuthFactor" Codec.bool      csresEmailAuthFactor
            <*> Codec.optionalField "active"          Codec.bool      csresActive
            <*> Codec.optionalField "status"          Codec.text      csresStatus

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.server.createSession@ using the given XRPC client.
createSession
  :: XrpcClient c
  => c
  -> CreateSessionRequest
  -> IO (Either XrpcError CreateSessionResponse)
createSession client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.server.createSession"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (LexJson.encode createSessionRequestCodec req)
    , xrpcReqHeaders = Map.singleton "Content-Type" "application/json"
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError CreateSessionResponse
parseResponse body =
  case LexJson.decode createSessionResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
