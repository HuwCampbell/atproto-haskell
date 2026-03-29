-- | Typed bindings for @com.atproto.server.*@ XRPC methods.
--
-- Types and codecs shared between client and server for the session
-- and account management endpoints.
module ATProto.Repo.Server
  ( -- * @com.atproto.server.describeServer@
    DescribeServerResponse (..)
  , describeServerResponseCodec
    -- * @com.atproto.server.createSession@
  , CreateSessionRequest (..)
  , createSessionRequestCodec
  , CreateSessionResponse (..)
  , createSessionResponseCodec
    -- * @com.atproto.server.getSession@
  , GetSessionResponse (..)
  , getSessionResponseCodec
  ) where

import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- ---------------------------------------------------------------------------
-- com.atproto.server.describeServer
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.server.describeServer@.
data DescribeServerResponse = DescribeServerResponse
  { dsrAvailableUserDomains :: [T.Text]
  , dsrInviteCodeRequired   :: Bool
  } deriving (Eq, Show)

describeServerResponseCodec :: Codec DescribeServerResponse
describeServerResponseCodec =
    Codec.record "com.atproto.server.describeServer#output" $
        DescribeServerResponse
            <$> Codec.requiredField "availableUserDomains" (Codec.array Codec.text) dsrAvailableUserDomains
            <*> Codec.requiredField "inviteCodeRequired"   Codec.bool              dsrInviteCodeRequired

-- ---------------------------------------------------------------------------
-- com.atproto.server.createSession
-- ---------------------------------------------------------------------------

-- | Input body for @com.atproto.server.createSession@.
data CreateSessionRequest = CreateSessionRequest
  { csrqIdentifier :: T.Text
  , csrqPassword   :: T.Text
  } deriving (Eq, Show)

createSessionRequestCodec :: Codec CreateSessionRequest
createSessionRequestCodec =
    Codec.record "com.atproto.server.createSession" $
        CreateSessionRequest
            <$> Codec.requiredField "identifier" Codec.text csrqIdentifier
            <*> Codec.requiredField "password"   Codec.text csrqPassword

-- | Response from @com.atproto.server.createSession@.
data CreateSessionResponse = CreateSessionResponse
  { csrDid        :: T.Text
  , csrHandle     :: T.Text
  , csrAccessJwt  :: T.Text
  , csrRefreshJwt :: T.Text
  } deriving (Eq, Show)

createSessionResponseCodec :: Codec CreateSessionResponse
createSessionResponseCodec =
    Codec.record "com.atproto.server.createSession#output" $
        CreateSessionResponse
            <$> Codec.requiredField "did"        Codec.did    csrDid
            <*> Codec.requiredField "handle"     Codec.handle csrHandle
            <*> Codec.requiredField "accessJwt"  Codec.text   csrAccessJwt
            <*> Codec.requiredField "refreshJwt" Codec.text   csrRefreshJwt

-- ---------------------------------------------------------------------------
-- com.atproto.server.getSession
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.server.getSession@.
data GetSessionResponse = GetSessionResponse
  { gsrDid    :: T.Text
  , gsrHandle :: T.Text
  } deriving (Eq, Show)

getSessionResponseCodec :: Codec GetSessionResponse
getSessionResponseCodec =
    Codec.record "com.atproto.server.getSession#output" $
        GetSessionResponse
            <$> Codec.requiredField "did"    Codec.did    gsrDid
            <*> Codec.requiredField "handle" Codec.handle gsrHandle
