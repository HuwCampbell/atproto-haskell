-- | AT Protocol identifier syntax – top-level re-export module.
--
-- Re-exports the most commonly needed definitions from all sub-modules.
-- Import this module for convenient access to every identifier type:
--
-- @
-- import ATProto.Syntax
-- @
--
-- Or import individual sub-modules for finer-grained control:
--
-- @
-- import ATProto.Syntax.DID
-- import ATProto.Syntax.Handle
-- @
module ATProto.Syntax
  ( -- * DIDs
    module ATProto.Syntax.DID
    -- * Handles
  , module ATProto.Syntax.Handle
    -- * NSIDs
  , module ATProto.Syntax.NSID
    -- * AT URIs
  , module ATProto.Syntax.AtUri
    -- * TIDs
  , module ATProto.Syntax.TID
    -- * Record keys
  , module ATProto.Syntax.RecordKey
  ) where

import ATProto.Syntax.DID
import ATProto.Syntax.Handle
import ATProto.Syntax.NSID
import ATProto.Syntax.AtUri
import ATProto.Syntax.TID
import ATProto.Syntax.RecordKey
