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
module Statusphere.Types
  ( StatusView (..)

  , statusViewCodec
  ) where

import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | A subset of the @app.bsky.actor.defs#profileViewDetailed@ schema.
--
-- Only the fields most commonly needed are decoded here.
data StatusView = StatusView
  { svStatus         :: T.Text
    -- ^ The account's DID.
  , scCreatedAt      :: T.Text
    -- ^ Number of posts, when available.
  } deriving (Eq, Show)

-- | Codec for the @getProfile@ response body.
statusViewCodec :: Codec StatusView
statusViewCodec =
    Codec.record "xyz.statusphere.status" $
        StatusView
            <$> Codec.requiredField "status"         Codec.text     svStatus
            <*> Codec.requiredField "createdAt"      Codec.datetime scCreatedAt
