-- | DPoP-aware XRPC client for ATProto OAuth sessions.
--
-- After completing the OAuth flow, use 'newOAuthXrpcClient' to obtain an
-- 'OAuthXrpcClient' that transparently handles:
--
-- * DPoP proof generation per request
-- * Per-PDS DPoP nonce caching and automatic update
-- * @use_dpop_nonce@ retries
-- * Live access-token refresh via 'getTokenInfo'
--
-- = Typical usage
--
-- @
-- import ATProto.OAuth
-- import ATProto.XRPC.Http  -- for the Manager
--
-- fetchProfile :: Manager -> OAuthClient -> Session -> IO ()
-- fetchProfile mgr oauthClient session = do
--   let did = tsSub (sessTokenSet session)
--   xrpcClient <- newOAuthXrpcClient mgr oauthClient did session
--   result     <- runXrpc xrpcClient someRequest
--   ...
-- @
module ATProto.OAuth.XrpcClient
  ( -- * Client
    OAuthXrpcClient
  , newOAuthXrpcClient
  ) where

import           Control.Exception    (SomeException, catch)
import           Data.IORef           (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Network.HTTP.Client  (Manager)

import           ATProto.OAuth.Client  (OAuthClient, Session (..), getTokenInfo)
import           ATProto.OAuth.DPoP    (DpopClaims (..), DpopKey,
                                        accessTokenHash, createDpopProof)
import           ATProto.OAuth.Types   (OAuthError (..), TokenSet (..))
import           ATProto.XRPC.Client   (XrpcClient (..))
import           ATProto.XRPC.Types    (XrpcError (..), XrpcMethod (..),
                                        XrpcRequest (..), XrpcResponse (..))
import           ATProto.XRPC.Http     (HttpXrpcClient, newHttpXrpcClientWith)

-- ---------------------------------------------------------------------------
-- Data type
-- ---------------------------------------------------------------------------

-- | An opaque XRPC client that handles DPoP automatically for a session.
--
-- Wraps an 'HttpXrpcClient' pointed at the PDS, attaches DPoP proofs and
-- 'Authorization: DPoP' headers on every request, caches the per-PDS
-- nonce, and refreshes the access token transparently.
data OAuthXrpcClient = OAuthXrpcClient
  { oacInner       :: HttpXrpcClient
    -- ^ Underlying HTTP XRPC client pointed at the PDS.
  , oacPdsUrl      :: T.Text
    -- ^ PDS base URL (for constructing the DPoP @htu@ claim).
  , oacDpopKey     :: DpopKey
    -- ^ Session DPoP keypair.
  , oacOAuthClient :: OAuthClient
    -- ^ OAuth client used for token refresh via 'getTokenInfo'.
  , oacDid         :: T.Text
    -- ^ DID of the authenticated user.
  , oacNonceRef    :: IORef (Maybe T.Text)
    -- ^ Cached DPoP nonce for this PDS, updated from response headers.
  }

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

-- | Create an 'OAuthXrpcClient' from a 'Session'.
--
-- The returned client handles DPoP proof generation, per-PDS nonce caching,
-- @use_dpop_nonce@ retries, and transparent token refresh via 'getTokenInfo'.
newOAuthXrpcClient
  :: Manager      -- ^ Shared TLS manager
  -> OAuthClient  -- ^ For token refresh
  -> T.Text       -- ^ DID of the authenticated user
  -> Session      -- ^ Session from 'callback'
  -> IO OAuthXrpcClient
newOAuthXrpcClient mgr oauthClient did session = do
  nonceRef <- newIORef Nothing
  let pdsUrl = tsAud (sessTokenSet session)
  return OAuthXrpcClient
    { oacInner       = newHttpXrpcClientWith mgr pdsUrl
    , oacPdsUrl      = pdsUrl
    , oacDpopKey     = sessDpopKey session
    , oacOAuthClient = oauthClient
    , oacDid         = did
    , oacNonceRef    = nonceRef
    }

-- ---------------------------------------------------------------------------
-- XrpcClient instance
-- ---------------------------------------------------------------------------

instance XrpcClient OAuthXrpcClient where
  runXrpc client req = do
    result <- runOAuthXrpc client req
    case result of
      Right resp -> return (Right resp)
      Left err
        | xrpcErrError err == "use_dpop_nonce" -> do
            -- Extract the nonce from the error response and retry once.
            let mNonce = Map.lookup "dpop-nonce" (xrpcErrHeaders err)
            case mNonce of
              Nothing    -> return (Left err)
              Just nonce -> do
                writeIORef (oacNonceRef client) (Just nonce)
                runOAuthXrpc client req
        | otherwise -> return (Left err)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Execute one DPoP-protected XRPC request (no retry).
runOAuthXrpc :: OAuthXrpcClient -> XrpcRequest -> IO (Either XrpcError XrpcResponse)
runOAuthXrpc client req = do
  -- Get current (possibly refreshed) access token.
  eTs <- catch (getTokenInfo (oacOAuthClient client) (oacDid client))
               (\e -> return (Left (OAuthNetworkError (show (e :: SomeException)))))
  case eTs of
    Left oauthErr -> return $ Left XrpcError
      { xrpcErrError   = "OAuthError"
      , xrpcErrMessage = Just (T.pack (show oauthErr))
      , xrpcErrStatus  = 0
      , xrpcErrHeaders = Map.empty
      }
    Right ts -> do
      let accessToken = tsAccessToken ts
          httpMethod  = case xrpcReqMethod req of
                          XrpcQuery     -> "GET"
                          XrpcProcedure -> "POST"
          -- Resource URL without query string (RFC 9449 §4.2).
          resourceUrl = oacPdsUrl client <> "/xrpc/" <> xrpcReqNsid req
          ath         = TE.decodeUtf8Lenient (accessTokenHash accessToken)
      cachedNonce <- readIORef (oacNonceRef client)
      eProof <- createDpopProof (oacDpopKey client) DpopClaims
        { dcHtm   = httpMethod
        , dcHtu   = resourceUrl
        , dcNonce = cachedNonce
        , dcAth   = Just ath
        }
      case eProof of
        Left err -> return $ Left XrpcError
          { xrpcErrError   = "DpopProofError"
          , xrpcErrMessage = Just (T.pack err)
          , xrpcErrStatus  = 0
          , xrpcErrHeaders = Map.empty
          }
        Right proof -> do
          let hdrs = Map.union
                       (Map.fromList
                         [ ("DPoP",          proof)
                         , ("Authorization", "DPoP " <> accessToken)
                         ])
                       (xrpcReqHeaders req)
          result <- runXrpc (oacInner client) req { xrpcReqHeaders = hdrs }
          -- Update the nonce cache from the response headers (success or error).
          let respHeaders = case result of
                              Right resp -> xrpcRespHeaders resp
                              Left  e    -> xrpcErrHeaders e
          case Map.lookup "dpop-nonce" respHeaders of
            Just nonce -> writeIORef (oacNonceRef client) (Just nonce)
            Nothing    -> return ()
          return result
