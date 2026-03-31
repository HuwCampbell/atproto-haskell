-- | Re-export of @com.atproto.sync.getBlob@ from its original location.
--
-- This module provides a properly-namespaced import for the blob fetching
-- endpoint.  The original implementation lives in 'ATProto.Repo.GetBlob'
-- for backward compatibility.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/sync/getBlob.json>.
module ATProto.Sync.GetBlob
  ( -- * Request parameters
    GetBlobParams (..)
    -- * Client function
  , getBlob
  ) where

import ATProto.Repo.GetBlob (GetBlobParams (..), getBlob)
