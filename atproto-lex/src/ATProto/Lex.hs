-- | AT Protocol Lexicon codec combinators.
--
-- Re-exports the codec combinators, schema types, and JSON\/CBOR bridges.
--
-- Typical usage:
--
-- @
-- import           ATProto.Lex         (Codec)
-- import qualified ATProto.Lex.Codec   as Codec
-- import qualified ATProto.Lex.Json    as LexJson
--
-- data StrongRef = StrongRef { srUri :: Text, srCid :: Text }
--
-- strongRefCodec :: Codec StrongRef
-- strongRefCodec =
--     Codec.record \"com.atproto.repo.strongRef\" $
--         StrongRef
--             \<$\> Codec.requiredField \"uri\" Codec.atUri srUri
--             \<*\> Codec.requiredField \"cid\" Codec.text  srCid
-- @
module ATProto.Lex
  ( -- * Re-exports
    module ATProto.Lex.Codec
  , module ATProto.Lex.Schema
  ) where

import ATProto.Lex.Codec
import ATProto.Lex.Schema
