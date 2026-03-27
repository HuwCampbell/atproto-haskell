-- | Server environment and shared state for the PDS.
--
-- The 'Env' type bundles all shared state needed by the XRPC endpoint
-- handlers: storage backends, signing keys, account metadata, and
-- in-memory session\/preference stores.
module ATProto.PDS.Server.Env
  ( -- * Environment
    Env (..)
  , AppM
  , AccountInfo (..)
  , SessionInfo (..)
    -- * Construction
  , newEnv
    -- * Helpers
  , lookupDIDForIdentifier
  , lookupHandle
  ) where

import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.IORef
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T

import ATProto.Car.Cid               (CidBytes)
import ATProto.Crypto.Types          (PrivKey)
import ATProto.OAuth.Provider.Token  (SigningKey, generateSigningKey)
import ATProto.OAuth.Provider.DPoP.Nonce (NonceState, newNonceState)
import ATProto.Syntax.DID            (DID, unDID)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Per-account metadata.
data AccountInfo = AccountInfo
  { aiDid        :: DID
  , aiHandle     :: T.Text
  , aiPassword   :: T.Text
    -- ^ Plaintext password (sufficient for a development server;
    --   a production server would store a hash).
  , aiActive     :: Bool
  }

-- | Issued session (access + refresh token pair).
data SessionInfo = SessionInfo
  { siDid          :: T.Text
  , siHandle       :: T.Text
  , siAccessToken  :: T.Text
  , siRefreshToken :: T.Text
  } deriving (Eq, Show)

-- | Server environment.
data Env s = Env
  { envStore          :: s
    -- ^ Content-addressed block store + repo metadata.
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
  , envAccounts       :: IORef (Map.Map T.Text AccountInfo)
    -- ^ Account registry  (DID text → account).
  , envHandleIndex    :: IORef (Map.Map T.Text DID)
    -- ^ Handle → DID index.
  , envSessions       :: IORef (Map.Map T.Text SessionInfo)
    -- ^ Access-token → session.
  , envRefreshTokens  :: IORef (Map.Map T.Text SessionInfo)
    -- ^ Refresh-token → session.
  , envPreferences    :: IORef (Map.Map T.Text BL.ByteString)
    -- ^ DID text → preferences JSON.
  , envBlobs          :: IORef (Map.Map CidBytes BS.ByteString)
    -- ^ In-memory blob store (CID → bytes).
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
  accounts   <- newIORef Map.empty
  handles    <- newIORef Map.empty
  sessions   <- newIORef Map.empty
  refreshes  <- newIORef Map.empty
  prefs      <- newIORef Map.empty
  blobs      <- newIORef Map.empty
  return Env
    { envStore         = store
    , envSigningKey    = commitKey
    , envTokenKey      = tokenKey
    , envNonceState    = nonceState
    , envIssuer        = issuer
    , envHostname      = hostname
    , envAccounts      = accounts
    , envHandleIndex   = handles
    , envSessions      = sessions
    , envRefreshTokens = refreshes
    , envPreferences   = prefs
    , envBlobs         = blobs
    }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Resolve an identifier (handle or DID text) to a 'DID'.
lookupDIDForIdentifier :: Env s -> T.Text -> IO (Maybe DID)
lookupDIDForIdentifier env ident = do
  -- Try handle index first.
  idx <- readIORef (envHandleIndex env)
  case Map.lookup ident idx of
    Just did -> return (Just did)
    Nothing  -> do
      -- Try matching by DID text in accounts.
      accts <- readIORef (envAccounts env)
      case Map.lookup ident accts of
        Just ai -> return (Just (aiDid ai))
        Nothing -> return Nothing

-- | Look up the handle for a DID.
lookupHandle :: Env s -> DID -> IO (Maybe T.Text)
lookupHandle env did = do
  accts <- readIORef (envAccounts env)
  return $ aiHandle <$> Map.lookup (unDID did) accts
