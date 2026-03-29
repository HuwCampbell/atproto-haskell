-- | Server environment and shared state for the PDS.
--
-- The 'Env' type bundles all shared state needed by the XRPC endpoint
-- handlers: storage backends, signing keys, and session stores.
module ATProto.PDS.Server.Env
  ( -- * Environment
    Env (..)
  , AppM
  , SessionInfo (..)
    -- * Construction
  , newEnv
  ) where

import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Map.Strict      as Map
import           Data.IORef
import qualified Data.Text            as T

import ATProto.Crypto.Types          (PrivKey)
import ATProto.OAuth.Provider.Token  (SigningKey, generateSigningKey)
import ATProto.OAuth.Provider.DPoP.Nonce (NonceState, newNonceState)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Issued session (access + refresh token pair).
data SessionInfo = SessionInfo
  { siDid          :: T.Text
  , siHandle       :: T.Text
  , siAccessToken  :: T.Text
  , siRefreshToken :: T.Text
  } deriving (Eq, Show)

-- | Server environment.
--
-- The type parameter @s@ is the storage backend which should implement
-- 'BlockStore', 'RepoStore', 'BlobStore', 'PreferenceStore', and
-- 'AccountStore' from "ATProto.PDS.Storage".
data Env s = Env
  { envStore          :: s
    -- ^ Storage backend (blocks, repos, blobs, preferences, accounts).
  , envSigningKey     :: PrivKey
    -- ^ Repo commit signing key.
  , envTokenKey       :: SigningKey
    -- ^ OAuth access-token signing key.
  , envNonceState     :: NonceState
    -- ^ DPoP nonce rotation state.
  , envIssuer         :: T.Text
    -- ^ Canonical issuer URL.
  , envHostname       :: T.Text
    -- ^ Server hostname (for handle resolution).
  , envSessions       :: IORef (Map.Map T.Text SessionInfo)
    -- ^ Access-token → session (in-memory; sessions are ephemeral).
  , envRefreshTokens  :: IORef (Map.Map T.Text SessionInfo)
    -- ^ Refresh-token → session.
  }

-- | The application monad: a reader over the server environment.
type AppM s = ReaderT (Env s) IO

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Create a new server environment.
newEnv
  :: s            -- ^ Storage backend
  -> PrivKey      -- ^ Repo commit signing key
  -> T.Text       -- ^ Issuer URL
  -> T.Text       -- ^ Server hostname
  -> IO (Env s)
newEnv store commitKey issuer hostname = do
  tokenKey   <- generateSigningKey
  nonceState <- newNonceState Nothing Nothing
  sessions   <- newIORef Map.empty
  refreshes  <- newIORef Map.empty
  return Env
    { envStore         = store
    , envSigningKey    = commitKey
    , envTokenKey      = tokenKey
    , envNonceState    = nonceState
    , envIssuer        = issuer
    , envHostname      = hostname
    , envSessions      = sessions
    , envRefreshTokens = refreshes
    }
