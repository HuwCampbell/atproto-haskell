-- | Typed binding for @com.atproto.server.checkAccountStatus@.
--
-- Returns the status of an account, especially as pertaining to import or
-- recovery.  Can be called many times over the course of an account migration.
-- Requires auth.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/server/checkAccountStatus.json>.
module ATProto.Server.CheckAccountStatus
  ( -- * Response
    CheckAccountStatusResponse (..)
    -- * Codecs
  , checkAccountStatusResponseCodec
    -- * Client function
  , checkAccountStatus
  ) where

import           Data.Int                (Int64)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import qualified ATProto.Lex.Schema      as Codec
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.server.checkAccountStatus@.
data CheckAccountStatusResponse = CheckAccountStatusResponse
  { casActivated         :: Bool
    -- ^ Whether the account is activated.
  , casValidDid          :: Bool
    -- ^ Whether the DID is valid.
  , casRepoCommit        :: T.Text
    -- ^ Current repo commit CID.
  , casRepoRev           :: T.Text
    -- ^ Current repo revision.
  , casRepoBlocks        :: Int64
    -- ^ Number of blocks in the repo.
  , casIndexedRecords    :: Int64
    -- ^ Number of indexed records.
  , casPrivateStateValues :: Int64
    -- ^ Number of private state values.
  , casExpectedBlobs     :: Int64
    -- ^ Number of expected blobs.
  , casImportedBlobs     :: Int64
    -- ^ Number of imported blobs.
  } deriving (Eq, Show)

-- | Codec for the @checkAccountStatus@ response body.
checkAccountStatusResponseCodec :: Codec CheckAccountStatusResponse
checkAccountStatusResponseCodec =
    Codec.record "com.atproto.server.checkAccountStatus#response" $
        CheckAccountStatusResponse
            <$> Codec.requiredField "activated"         Codec.bool                          casActivated
            <*> Codec.requiredField "validDid"          Codec.bool                          casValidDid
            <*> Codec.requiredField "repoCommit"        (Codec.string Codec.LexFormatCid)   casRepoCommit
            <*> Codec.requiredField "repoRev"           Codec.text                          casRepoRev
            <*> Codec.requiredField "repoBlocks"        Codec.int                           casRepoBlocks
            <*> Codec.requiredField "indexedRecords"    Codec.int                           casIndexedRecords
            <*> Codec.requiredField "privateStateValues" Codec.int                          casPrivateStateValues
            <*> Codec.requiredField "expectedBlobs"     Codec.int                           casExpectedBlobs
            <*> Codec.requiredField "importedBlobs"     Codec.int                           casImportedBlobs

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.server.checkAccountStatus@ using the given XRPC client.
--
-- Requires auth.
checkAccountStatus
  :: XrpcClient c
  => c
  -> IO (Either XrpcError CheckAccountStatusResponse)
checkAccountStatus client = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.server.checkAccountStatus"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError CheckAccountStatusResponse
parseResponse body =
  case LexJson.decode checkAccountStatusResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
