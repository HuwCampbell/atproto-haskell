-- | Typed binding for @com.atproto.server.createAccount@.
--
-- Create an account.  Implemented by PDS.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/server/createAccount.json>.
module ATProto.Server.CreateAccount
  ( -- * Request
    CreateAccountRequest (..)
    -- * Response
  , CreateAccountResponse (..)
    -- * Codecs
  , createAccountRequestCodec
  , createAccountResponseCodec
    -- * Client function
  , createAccount
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

-- | Input body for @com.atproto.server.createAccount@.
data CreateAccountRequest = CreateAccountRequest
  { careqHandle           :: T.Text
    -- ^ Requested handle for the account.
  , careqEmail            :: Maybe T.Text
    -- ^ Account email.
  , careqDid              :: Maybe T.Text
    -- ^ Pre-existing atproto DID, being imported to a new account.
  , careqInviteCode       :: Maybe T.Text
    -- ^ Invite code.
  , careqVerificationCode :: Maybe T.Text
    -- ^ Verification code.
  , careqVerificationPhone :: Maybe T.Text
    -- ^ Verification phone number.
  , careqPassword         :: Maybe T.Text
    -- ^ Initial account password.
  , careqRecoveryKey      :: Maybe T.Text
    -- ^ DID PLC rotation key (recovery key).
  , careqPlcOp            :: Maybe LexValue
    -- ^ A signed DID PLC operation for account migration.
  } deriving (Eq, Show)

-- | Codec for the @createAccount@ request body.
createAccountRequestCodec :: Codec CreateAccountRequest
createAccountRequestCodec =
    Codec.record "com.atproto.server.createAccount#request" $
        CreateAccountRequest
            <$> Codec.requiredField "handle"            Codec.handle    careqHandle
            <*> Codec.optionalField "email"             Codec.text      careqEmail
            <*> Codec.optionalField "did"               Codec.did       careqDid
            <*> Codec.optionalField "inviteCode"        Codec.text      careqInviteCode
            <*> Codec.optionalField "verificationCode"  Codec.text      careqVerificationCode
            <*> Codec.optionalField "verificationPhone" Codec.text      careqVerificationPhone
            <*> Codec.optionalField "password"          Codec.text      careqPassword
            <*> Codec.optionalField "recoveryKey"       Codec.text      careqRecoveryKey
            <*> Codec.optionalField "plcOp"             Codec.lexValue  careqPlcOp

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.server.createAccount@.
data CreateAccountResponse = CreateAccountResponse
  { caresAccessJwt  :: T.Text
    -- ^ Short-lived access token.
  , caresRefreshJwt :: T.Text
    -- ^ Longer-lived refresh token.
  , caresHandle     :: T.Text
    -- ^ The new account handle.
  , caresDid        :: T.Text
    -- ^ The DID of the new account.
  , caresDidDoc     :: Maybe LexValue
    -- ^ Complete DID document.
  } deriving (Eq, Show)

-- | Codec for the @createAccount@ response body.
createAccountResponseCodec :: Codec CreateAccountResponse
createAccountResponseCodec =
    Codec.record "com.atproto.server.createAccount#response" $
        CreateAccountResponse
            <$> Codec.requiredField "accessJwt"  Codec.text      caresAccessJwt
            <*> Codec.requiredField "refreshJwt" Codec.text      caresRefreshJwt
            <*> Codec.requiredField "handle"     Codec.handle    caresHandle
            <*> Codec.requiredField "did"        Codec.did       caresDid
            <*> Codec.optionalField "didDoc"     Codec.lexValue  caresDidDoc

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.server.createAccount@ using the given XRPC client.
createAccount
  :: XrpcClient c
  => c
  -> CreateAccountRequest
  -> IO (Either XrpcError CreateAccountResponse)
createAccount client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.server.createAccount"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (LexJson.encode createAccountRequestCodec req)
    , xrpcReqHeaders = Map.singleton "Content-Type" "application/json"
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError CreateAccountResponse
parseResponse body =
  case LexJson.decode createAccountResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
