-- | Typed binding for @com.atproto.identity.updateHandle@.
--
-- Updates the current account's handle.  Verifies handle validity, and
-- updates did:plc document if necessary.  Implemented by PDS, and requires
-- auth.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/identity/updateHandle.json>.
module ATProto.Identity.UpdateHandle
  ( -- * Request
    UpdateHandleRequest (..)
    -- * Codecs
  , updateHandleRequestCodec
    -- * Client function
  , updateHandle
  ) where

import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Input body for @com.atproto.identity.updateHandle@.
newtype UpdateHandleRequest = UpdateHandleRequest
  { uhrHandle :: T.Text
    -- ^ The new handle.
  } deriving (Eq, Show)

-- | Codec for the @updateHandle@ request body.
updateHandleRequestCodec :: Codec UpdateHandleRequest
updateHandleRequestCodec =
    Codec.record "com.atproto.identity.updateHandle#request" $
        UpdateHandleRequest
            <$> Codec.requiredField "handle" Codec.handle uhrHandle

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.identity.updateHandle@ using the given XRPC client.
--
-- Requires auth.  Returns 'Right ()' on success (no response body).
updateHandle
  :: XrpcClient c
  => c
  -> UpdateHandleRequest
  -> IO (Either XrpcError ())
updateHandle client req = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcProcedure
    , xrpcReqNsid    = "com.atproto.identity.updateHandle"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Just (LexJson.encode updateHandleRequestCodec req)
    , xrpcReqHeaders = Map.singleton "Content-Type" "application/json"
    }
  case result of
    Left  err -> return (Left err)
    Right _   -> return (Right ())
