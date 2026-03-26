-- | Typed binding for @com.atproto.repo.createRecord@.
--
-- This module provides Haskell types for the request and JSON response of
-- the @com.atproto.repo.createRecord@ XRPC procedure, plus a convenience
-- function 'createRecord' that works with any 'XrpcClient'.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/createRecord.json>.
module ATProto.Repo.CreateRecord
  ( -- * Request
    CreateRecordRequest (..)
    -- * Response
  , CreateRecordResponse (..)
    -- * Codec
  , createRecordRequestCodec
  , createRecordResponseCodec
    -- * Client function
  , createRecord
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.Repo.CommitMeta (CommitMeta, commitMetaCodec)
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Input body for @com.atproto.repo.createRecord@.
data CreateRecordRequest = CreateRecordRequest
  { crrRepo       :: T.Text
    -- ^ The DID of the repository (usually the authenticated user).
  , crrCollection :: T.Text
    -- ^ NSID of the collection, e.g. @\"xyz.statusphere.status\"@.
  , crrRkey       :: T.Text
    -- ^ Record key (e.g. a TID).
  , crrRecord     :: LexValue
    -- ^ The record value to write (including @$type@).
  , crrValidate   :: Maybe Bool
    -- ^ Whether to validate the record against the Lexicon.
  } deriving (Eq, Show)


createRecordRequestCodec :: Codec CreateRecordRequest
createRecordRequestCodec =
    Codec.record "com.atproto.repo.createRecord" $
        CreateRecordRequest
            <$> Codec.requiredField "repo"       Codec.atUri     crrRepo
            <*> Codec.requiredField "collection" Codec.text      crrCollection
            <*> Codec.requiredField "rkey"       Codec.text      crrRkey
            <*> Codec.requiredField "record"     Codec.lexValue  crrRecord
            <*> Codec.optionalField "validate"   Codec.bool      crrValidate


-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.repo.createRecord@.
data CreateRecordResponse = CreateRecordResponse
  { crUri :: T.Text
    -- ^ AT-URI of the written record.
  , crCid :: T.Text
    -- ^ CID of the written record.
  , crCommit :: CommitMeta
    -- ^ Commit metadata
  } deriving (Eq, Show)

-- | Codec for the @createRecord@ response body.
createRecordResponseCodec :: Codec CreateRecordResponse
createRecordResponseCodec =
    Codec.record "com.atproto.repo.createRecord#response" $
        CreateRecordResponse
            <$> Codec.requiredField "uri"    Codec.atUri     crUri
            <*> Codec.requiredField "cid"    Codec.text      crCid
            <*> Codec.requiredField "commit" commitMetaCodec crCommit

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.repo.createRecord@ using the given XRPC client.
--
-- Writes a record to the user's repository and returns the AT-URI and CID.
createRecord
  :: XrpcClient c
  => c
  -> CreateRecordRequest
  -> IO (Either XrpcError CreateRecordResponse)
createRecord client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.repo.createRecord"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (LexJson.encode createRecordRequestCodec req)
    , xrpcReqHeaders = Map.singleton "Content-Type" "application/json"
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

-- | Decode the JSON response body.
parseResponse :: BL.ByteString -> Either XrpcError CreateRecordResponse
parseResponse body =
  case LexJson.decode createRecordResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
