-- | Typed binding for @com.atproto.repo.deleteRecord@.
--
-- This module provides Haskell types for the request and JSON response of
-- the @com.atproto.repo.deleteRecord@ XRPC procedure, plus a convenience
-- function 'deleteRecord' that works with any 'XrpcClient'.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/deleteRecord.json>.
module ATProto.Repo.DeleteRecord
  ( -- * Request
    DeleteRecordRequest (..)
    -- * Response
  , DeleteRecordResponse (..)
    -- * Codec
  , deleteRecordRequestCodec
  , deleteRecordResponseCodec
    -- * Client function
  , deleteRecord
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.Repo.CommitMeta (CommitMeta, commitMetaCodec)
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Input body for @com.atproto.repo.deleteRecord@.
data DeleteRecordRequest = DeleteRecordRequest
  { drrRepo       :: T.Text
    -- ^ The DID of the repository (usually the authenticated user).
  , drrCollection :: T.Text
    -- ^ NSID of the collection, e.g. @\"xyz.statusphere.status\"@.
  , drrRkey       :: T.Text
    -- ^ Record key (e.g. a TID).
  } deriving (Eq, Show)


deleteRecordRequestCodec :: Codec DeleteRecordRequest
deleteRecordRequestCodec =
    Codec.record "com.atproto.repo.deleteRecord" $
        DeleteRecordRequest
            <$> Codec.requiredField "repo"       Codec.atUri     drrRepo
            <*> Codec.requiredField "collection" Codec.text      drrCollection
            <*> Codec.requiredField "rkey"       Codec.text      drrRkey


-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.repo.deleteRecord@.
newtype DeleteRecordResponse = DeleteRecordResponse {
    drrCommit :: CommitMeta
  } deriving (Eq, Show)


-- | Codec for the @deleteRecord@ response body.
deleteRecordResponseCodec :: Codec DeleteRecordResponse
deleteRecordResponseCodec =
    Codec.record "com.atproto.repo.deleteRecord#response" $
        DeleteRecordResponse
            <$> Codec.requiredField "commit" commitMetaCodec drrCommit

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.repo.deleteRecord@ using the given XRPC client.
--
-- Writes a record to the user's repository and returns the AT-URI and CID.
deleteRecord
  :: XrpcClient c
  => c
  -> DeleteRecordRequest
  -> IO (Either XrpcError DeleteRecordResponse)
deleteRecord client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.repo.deleteRecord"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (LexJson.encode deleteRecordRequestCodec req)
    , xrpcReqHeaders = Map.singleton "Content-Type" "application/json"
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

-- | Decode the JSON response body.
parseResponse :: BL.ByteString -> Either XrpcError DeleteRecordResponse
parseResponse body =
  case LexJson.decode deleteRecordResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
