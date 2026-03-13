-- | Typed binding for @app.bsky.actor.getProfile@.
--
-- This module provides Haskell types for the query parameters and JSON
-- response of the @app.bsky.actor.getProfile@ XRPC method, plus a
-- convenience function 'getProfile' that works with any 'XrpcClient'.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/actor/getProfile.json>.
--
-- __Note:__ This binding is a temporary hand-written stub.  Once a Lexicon
-- code-generator is available the module will be superseded by the
-- generated version.
module ATProto.Repo.GetProfile
  ( -- * Request parameters
    GetProfileParams (..)
    -- * Response
  , ProfileView (..)
    -- * Codec
  , profileViewCodec
    -- * Client function
  , getProfile
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import           Data.Int                (Int64)

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import ATProto.XRPC.Client (XrpcClient (..))
import ATProto.XRPC.Types  (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Query parameters for @app.bsky.actor.getProfile@.
data GetProfileParams = GetProfileParams
  { gppActor :: T.Text
    -- ^ The DID or handle of the account to fetch, e.g.
    -- @\"did:plc:ewvi7nxzyoun6zhhandbv25b\"@ or @\"haileyok.com\"@.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | A subset of the @app.bsky.actor.defs#profileViewDetailed@ schema.
--
-- Only the fields most commonly needed are decoded here.
data ProfileView = ProfileView
  { pvDid            :: T.Text
    -- ^ The account's DID.
  , pvHandle         :: T.Text
    -- ^ The account's handle.
  , pvDisplayName    :: Maybe T.Text
    -- ^ Display name, if set.
  , pvDescription    :: Maybe T.Text
    -- ^ Profile description \/ bio, if set.
  , pvAvatar         :: Maybe T.Text
    -- ^ URL of the avatar image, if set.
  , pvFollowersCount :: Maybe Int64
    -- ^ Number of followers, when available.
  , pvFollowsCount   :: Maybe Int64
    -- ^ Number of accounts followed, when available.
  , pvPostsCount     :: Maybe Int64
    -- ^ Number of posts, when available.
  } deriving (Eq, Show)

-- | Codec for the @getProfile@ response body.
profileViewCodec :: Codec ProfileView
profileViewCodec =
    Codec.record "app.bsky.actor.defs#profileViewDetailed" $
        ProfileView
            <$> Codec.requiredField "did"            Codec.did    (.pvDid)
            <*> Codec.requiredField "handle"         Codec.handle (.pvHandle)
            <*> Codec.optionalField "displayName"    Codec.text   (.pvDisplayName)
            <*> Codec.optionalField "description"    Codec.text   (.pvDescription)
            <*> Codec.optionalField "avatar"         Codec.uri    (.pvAvatar)
            <*> Codec.optionalField "followersCount" Codec.int    (.pvFollowersCount)
            <*> Codec.optionalField "followsCount"   Codec.int    (.pvFollowsCount)
            <*> Codec.optionalField "postsCount"     Codec.int    (.pvPostsCount)

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @app.bsky.actor.getProfile@ using the given XRPC client.
--
-- Returns 'Right' with the parsed 'ProfileView' on success, or 'Left' with
-- an 'XrpcError' on failure.  The response body is decoded from JSON; a
-- parse failure is reported as an 'XrpcError' with the token @\"ParseError\"@.
--
-- = Example
--
-- @
-- import ATProto.Repo.GetProfile
-- import ATProto.XRPC.Http
--
-- main :: IO ()
-- main = do
--   client <- newHttpXrpcClient "https://public.api.bsky.app"
--   result <- getProfile client (GetProfileParams "haileyok.com")
--   case result of
--     Left  err  -> print err
--     Right prof -> do
--       putStrLn ("Handle: " ++ show (pvHandle prof))
--       putStrLn ("Name:   " ++ show (pvDisplayName prof))
-- @
getProfile
  :: XrpcClient c
  => c
  -> GetProfileParams
  -> IO (Either XrpcError ProfileView)
getProfile client params = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "app.bsky.actor.getProfile"
    , xrpcReqParams  = Map.fromList [("actor", gppActor params)]
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

-- | Decode the JSON response body into a 'ProfileView'.
parseResponse :: BL.ByteString -> Either XrpcError ProfileView
parseResponse body =
  case LexJson.decode profileViewCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
