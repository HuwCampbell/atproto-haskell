-- | Typed bindings for @com.atproto.identity.defs@.
--
-- Shared type definitions for the @com.atproto.identity.*@ namespace.
--
-- The Lexicon for these types is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/identity/defs.json>.
module ATProto.Identity.Defs
  ( -- * Types
    IdentityInfo (..)
    -- * Codecs
  , identityInfoCodec
  ) where

import qualified Data.Text               as T

import           ATProto.Ipld.Value      (LexValue)
import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- ---------------------------------------------------------------------------
-- IdentityInfo
-- ---------------------------------------------------------------------------

-- | Resolved identity information.
--
-- Corresponds to @com.atproto.identity.defs#identityInfo@.
data IdentityInfo = IdentityInfo
  { iiDid    :: T.Text
    -- ^ The DID of the identity.
  , iiHandle :: T.Text
    -- ^ The validated handle of the account; or @\"handle.invalid\"@ if the
    -- handle did not bi-directionally match the DID document.
  , iiDidDoc :: LexValue
    -- ^ The complete DID document for the identity.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.identity.defs#identityInfo@.
identityInfoCodec :: Codec IdentityInfo
identityInfoCodec =
    Codec.record "com.atproto.identity.defs#identityInfo" $
        IdentityInfo
            <$> Codec.requiredField "did"    Codec.did      iiDid
            <*> Codec.requiredField "handle" Codec.handle   iiHandle
            <*> Codec.requiredField "didDoc" Codec.lexValue iiDidDoc
