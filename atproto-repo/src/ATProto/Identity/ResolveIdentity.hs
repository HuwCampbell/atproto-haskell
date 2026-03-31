-- | Typed binding for @com.atproto.identity.resolveIdentity@.
--
-- Resolves an identity (DID or Handle) to a full identity (DID document
-- and verified handle).
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/identity/resolveIdentity.json>.
module ATProto.Identity.ResolveIdentity
  ( -- * Request parameters
    ResolveIdentityParams (..)
    -- * Re-exported response type
  , IdentityInfo (..)
    -- * Codecs
  , identityInfoCodec
    -- * Client function
  , resolveIdentity
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import qualified ATProto.Lex.Json        as LexJson
import           ATProto.Identity.Defs   (IdentityInfo (..), identityInfoCodec)
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request parameters
-- ---------------------------------------------------------------------------

-- | Query parameters for @com.atproto.identity.resolveIdentity@.
newtype ResolveIdentityParams = ResolveIdentityParams
  { ripIdentifier :: T.Text
    -- ^ Handle or DID to resolve.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.identity.resolveIdentity@ using the given XRPC client.
resolveIdentity
  :: XrpcClient c
  => c
  -> ResolveIdentityParams
  -> IO (Either XrpcError IdentityInfo)
resolveIdentity client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.identity.resolveIdentity"
    , xrpcReqParams  = Map.singleton "identifier" (ripIdentifier params)
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError IdentityInfo
parseResponse body =
  case LexJson.decode identityInfoCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
