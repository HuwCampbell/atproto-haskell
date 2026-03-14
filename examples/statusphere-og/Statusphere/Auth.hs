-- | OAuth client setup for the Statusphere example app.
--
-- Configures the ATProto OAuth client using a loopback client ID (for
-- development) and in-memory stores for state and sessions.  The cookie
-- layer in the server maps browser sessions to DIDs.
module Statusphere.Auth
  ( createOAuthClient
  ) where

import qualified Data.Text               as T
import           Network.HTTP.Client      (Manager)

import           ATProto.DID              (DidDocument (..), DidResolver (..),
                                           Service (..),
                                           defaultPlcResolver, newWebResolver)
import           ATProto.Identity         (defaultHandleResolverOpts,
                                           newHandleResolver, resolveHandle)
import           ATProto.OAuth            (newInMemorySessionStore,
                                           newInMemoryStateStore,
                                           newOAuthClient)
import           ATProto.OAuth.Client     (DidDocumentLike (..),
                                           OAuthClient,
                                           OAuthClientConfig (..))
import           ATProto.OAuth.Types      (OAuthClientMetadata (..),
                                           loopbackClientMetadata)

-- | Create an OAuth client configured for loopback development.
createOAuthClient :: Manager -> Int -> IO OAuthClient
createOAuthClient mgr port = do
  handleResolver <- newHandleResolver defaultHandleResolverOpts
  plcRes         <- defaultPlcResolver
  webRes         <- newWebResolver

  let callbackUrl = "http://127.0.0.1:" <> T.pack (show port) <> "/oauth/callback"
      clientMeta  = (loopbackClientMetadata "atproto transition:generic" [callbackUrl])
        { cmClientName = Just "Statusphere (Haskell)" }

  stateStore   <- newInMemoryStateStore
  sessionStore <- newInMemorySessionStore

  let cfg = OAuthClientConfig
        { occMetadata           = clientMeta
        , occManager            = mgr
        , occResolveHandle      = resolveHandle handleResolver
        , occResolveDid         = didResolver plcRes webRes
        , occTokenRefreshBuffer = 60
        }

  newOAuthClient cfg stateStore sessionStore

-- | Resolve a DID to a 'DidDocumentLike' (just the PDS URL).
didResolver
  :: (DidResolver plc, DidResolver web)
  => plc -> web -> T.Text -> IO (Either String DidDocumentLike)
didResolver plcRes webRes did
  | "did:plc:" `T.isPrefixOf` did = go plcRes
  | "did:web:" `T.isPrefixOf` did = go webRes
  | otherwise = return (Left ("Unsupported DID method: " ++ T.unpack did))
  where
    go :: DidResolver r => r -> IO (Either String DidDocumentLike)
    go resolver = do
      eDoc <- resolve resolver did
      case eDoc of
        Left err  -> return (Left (show err))
        Right doc ->
          case filter (\s -> serviceType s == "AtprotoPersonalDataServer") (didDocServices doc) of
            (s:_) -> return (Right (DidDocumentLike (serviceEndpoint s)))
            []    -> return (Left ("No PDS service in DID document for " ++ T.unpack did))
