-- | Typed binding for @com.atproto.repo.putRecord@.
--
-- This module provides Haskell types for the request and JSON response of
-- the @com.atproto.repo.putRecord@ XRPC procedure, plus a convenience
-- function 'putRecord' that works with any 'XrpcClient'.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/putRecord.json>.
module ATProto.Repo.PutRecord
  ( -- * Request
    PutRecordRequest (..)
    -- * Response
  , PutRecordResponse (..)
    -- * Codec
  , putRecordResponseCodec
    -- * Client function
  , putRecord
  ) where

import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client (XrpcClient (..))
import           ATProto.XRPC.Types  (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Input body for @com.atproto.repo.putRecord@.
data PutRecordRequest = PutRecordRequest
  { prrRepo       :: T.Text
    -- ^ The DID of the repository (usually the authenticated user).
  , prrCollection :: T.Text
    -- ^ NSID of the collection, e.g. @\"xyz.statusphere.status\"@.
  , prrRkey       :: T.Text
    -- ^ Record key (e.g. a TID).
  , prrRecord     :: Aeson.Value
    -- ^ The record value to write (including @$type@).
  , prrValidate   :: Maybe Bool
    -- ^ Whether to validate the record against the Lexicon.
  } deriving (Eq, Show)

-- | Serialise a 'PutRecordRequest' to a JSON body.
encodeRequest :: PutRecordRequest -> BL.ByteString
encodeRequest r = Aeson.encode $ Aeson.object $
  [ "repo"       Aeson..= prrRepo r
  , "collection" Aeson..= prrCollection r
  , "rkey"       Aeson..= prrRkey r
  , "record"     Aeson..= prrRecord r
  ] ++
  maybe [] (\v -> ["validate" Aeson..= v]) (prrValidate r)

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.repo.putRecord@.
data PutRecordResponse = PutRecordResponse
  { prUri :: T.Text
    -- ^ AT-URI of the written record.
  , prCid :: T.Text
    -- ^ CID of the written record.
  } deriving (Eq, Show)

-- | Codec for the @putRecord@ response body.
putRecordResponseCodec :: Codec PutRecordResponse
putRecordResponseCodec =
    Codec.record "com.atproto.repo.putRecord#response" $
        PutRecordResponse
            <$> Codec.requiredField "uri" Codec.atUri prUri
            <*> Codec.requiredField "cid" Codec.text  prCid

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.repo.putRecord@ using the given XRPC client.
--
-- Writes a record to the user's repository and returns the AT-URI and CID.
putRecord
  :: XrpcClient c
  => c
  -> PutRecordRequest
  -> IO (Either XrpcError PutRecordResponse)
putRecord client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.repo.putRecord"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (encodeRequest req)
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

-- | Decode the JSON response body.
parseResponse :: BL.ByteString -> Either XrpcError PutRecordResponse
parseResponse body =
  case LexJson.decode putRecordResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
