-- | Typed binding for @com.atproto.repo.uploadBlob@.
--
-- The Lexicon is at:
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/uploadBlob.json>
--
-- Upload a blob (image, video, etc.) to the PDS.  The binary body is sent
-- with the caller-supplied MIME type.  On success the server returns a
-- 'BlobRef' that can be embedded in Lexicon records wherever a @blob@ field
-- appears.
module ATProto.Repo.UploadBlob
  ( -- * Request
    UploadBlobRequest (..)
    -- * Response
  , UploadBlobResponse (..)
    -- * Codec
  , uploadBlobResponseCodec
    -- * Client function
  , uploadBlob
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Ipld.Value      (BlobRef)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client (XrpcClient (..))
import           ATProto.XRPC.Types  (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Input for @com.atproto.repo.uploadBlob@.
data UploadBlobRequest = UploadBlobRequest
  { ubrBody     :: BL.ByteString
    -- ^ Raw binary content of the blob.
  , ubrMimeType :: T.Text
    -- ^ MIME type, e.g. @\"image\/jpeg\"@.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.repo.uploadBlob@.
data UploadBlobResponse = UploadBlobResponse
  { ubrBlob :: BlobRef
    -- ^ The blob reference returned by the server.
  } deriving (Eq, Show)

-- | Codec for the @uploadBlob@ response body.
--
-- The server returns JSON of the form:
-- @{\"blob\": {\"$type\":\"blob\",\"ref\":{...},\"mimeType\":\"...\",\"size\":...}}@
uploadBlobResponseCodec :: Codec UploadBlobResponse
uploadBlobResponseCodec =
    Codec.record "com.atproto.repo.uploadBlob#response" $
        UploadBlobResponse
            <$> Codec.requiredField "blob" Codec.blob ubrBlob

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.repo.uploadBlob@ using the given XRPC client.
--
-- Sends the raw binary body with the given MIME type and returns the
-- 'BlobRef' on success.
uploadBlob
  :: XrpcClient c
  => c
  -> UploadBlobRequest
  -> IO (Either XrpcError UploadBlobResponse)
uploadBlob client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.repo.uploadBlob"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (ubrBody req)
    , xrpcReqHeaders = Map.singleton "Content-Type" (ubrMimeType req)
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

-- | Decode the JSON response body.
parseResponse :: BL.ByteString -> Either XrpcError UploadBlobResponse
parseResponse body =
  case LexJson.decode uploadBlobResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
