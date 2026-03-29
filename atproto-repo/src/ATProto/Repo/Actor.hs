-- | Typed bindings for @app.bsky.actor.*@ XRPC methods.
module ATProto.Repo.Actor
  ( -- * @app.bsky.actor.getPreferences@
    GetPreferencesResponse (..)
  , getPreferencesResponseCodec
  ) where

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- | Response from @app.bsky.actor.getPreferences@.
newtype GetPreferencesResponse = GetPreferencesResponse
  { gprPreferences :: [LexValue]
  } deriving (Eq, Show)

getPreferencesResponseCodec :: Codec GetPreferencesResponse
getPreferencesResponseCodec =
    Codec.record "app.bsky.actor.defs#preferences" $
        GetPreferencesResponse
            <$> Codec.requiredField "preferences" (Codec.array Codec.lexValue) gprPreferences
