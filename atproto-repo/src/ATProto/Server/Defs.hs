-- | Typed bindings for @com.atproto.server.defs@.
--
-- Shared type definitions for the @com.atproto.server.*@ namespace.
--
-- The Lexicon for these types is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/server/defs.json>.
module ATProto.Server.Defs
  ( -- * Types
    InviteCode (..)
  , InviteCodeUse (..)
    -- * Codecs
  , inviteCodeCodec
  , inviteCodeUseCodec
  ) where

import           Data.Int                (Int64)
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- ---------------------------------------------------------------------------
-- InviteCode
-- ---------------------------------------------------------------------------

-- | An invite code on the server.
--
-- Corresponds to @com.atproto.server.defs#inviteCode@.
data InviteCode = InviteCode
  { icCode       :: T.Text
    -- ^ The invite code string.
  , icAvailable  :: Int64
    -- ^ Number of uses still available.
  , icDisabled   :: Bool
    -- ^ Whether the code has been disabled.
  , icForAccount :: T.Text
    -- ^ Account this code was created for.
  , icCreatedBy  :: T.Text
    -- ^ DID or identifier of the creator.
  , icCreatedAt  :: T.Text
    -- ^ Timestamp when this code was created.
  , icUses       :: [InviteCodeUse]
    -- ^ List of accounts that have used this code.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.server.defs#inviteCode@.
inviteCodeCodec :: Codec InviteCode
inviteCodeCodec =
    Codec.record "com.atproto.server.defs#inviteCode" $
        InviteCode
            <$> Codec.requiredField "code"       Codec.text                            icCode
            <*> Codec.requiredField "available"  Codec.int                             icAvailable
            <*> Codec.requiredField "disabled"   Codec.bool                            icDisabled
            <*> Codec.requiredField "forAccount" Codec.text                            icForAccount
            <*> Codec.requiredField "createdBy"  Codec.text                            icCreatedBy
            <*> Codec.requiredField "createdAt"  Codec.datetime                        icCreatedAt
            <*> Codec.requiredField "uses"       (Codec.array inviteCodeUseCodec)      icUses

-- ---------------------------------------------------------------------------
-- InviteCodeUse
-- ---------------------------------------------------------------------------

-- | A record of an invite code being used.
--
-- Corresponds to @com.atproto.server.defs#inviteCodeUse@.
data InviteCodeUse = InviteCodeUse
  { icuUsedBy :: T.Text
    -- ^ DID of the account that used the code.
  , icuUsedAt :: T.Text
    -- ^ Timestamp when the code was used.
  } deriving (Eq, Show)

-- | Codec for @com.atproto.server.defs#inviteCodeUse@.
inviteCodeUseCodec :: Codec InviteCodeUse
inviteCodeUseCodec =
    Codec.record "com.atproto.server.defs#inviteCodeUse" $
        InviteCodeUse
            <$> Codec.requiredField "usedBy" Codec.did      icuUsedBy
            <*> Codec.requiredField "usedAt" Codec.datetime  icuUsedAt
