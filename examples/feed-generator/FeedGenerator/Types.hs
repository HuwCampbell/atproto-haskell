-- | Application types for the feed generator example.
--
-- Defines the environment record and application monad used throughout
-- the feed generator handlers.
module FeedGenerator.Types
  ( ServiceDid
  , AppEnv (..)
  , AppM
  ) where

import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Text                  as T

import           ATProto.DID                (CachingResolver, DispatchResolver)

-- | The DID of this feed generator service.
type ServiceDid = T.Text

-- | Application environment threaded through handlers.
data AppEnv = AppEnv
  { envServiceDid  :: ServiceDid
    -- ^ This feed generator's own DID (used as the expected audience).
  , envFeedUri     :: T.Text
    -- ^ The AT-URI of the feed this generator serves.
  , envDidResolver :: CachingResolver DispatchResolver
    -- ^ Caching DID resolver used to verify service-auth JWTs.
  }

-- | The application monad.
type AppM = ReaderT AppEnv IO
