-- | Lexicon schema JSON parsing.
--
-- Provides 'FromJSON' / 'ToJSON' instances for all Lexicon schema types so
-- that @.lexicon.json@ files can be decoded directly into 'LexiconDoc'
-- values.
module ATProto.Lexicon.Json
  ( -- * Re-exports
    module ATProto.Lexicon.Types
  ) where

import ATProto.Lexicon.Types
