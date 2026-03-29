-- | Typed bindings for @chat.bsky.convo.*@ XRPC methods.
module ATProto.Repo.Chat
  ( -- * @chat.bsky.convo.getLog@
    GetLogResponse (..)
  , getLogResponseCodec
    -- * @chat.bsky.convo.listConvos@
  , ListConvosResponse (..)
  , listConvosResponseCodec
  ) where

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- | Response from @chat.bsky.convo.getLog@.
newtype GetLogResponse = GetLogResponse
  { glrLogs :: [LexValue]
  } deriving (Eq, Show)

getLogResponseCodec :: Codec GetLogResponse
getLogResponseCodec =
    Codec.record "chat.bsky.convo.getLog#output" $
        GetLogResponse
            <$> Codec.requiredField "logs" (Codec.array Codec.lexValue) glrLogs

-- | Response from @chat.bsky.convo.listConvos@.
newtype ListConvosResponse = ListConvosResponse
  { lcrConvos :: [LexValue]
  } deriving (Eq, Show)

listConvosResponseCodec :: Codec ListConvosResponse
listConvosResponseCodec =
    Codec.record "chat.bsky.convo.listConvos#output" $
        ListConvosResponse
            <$> Codec.requiredField "convos" (Codec.array Codec.lexValue) lcrConvos
