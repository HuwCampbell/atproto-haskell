-- | Authentication verifier for the PDS XRPC server.
--
-- Provides an 'AuthVerifier' that accepts both:
--
-- * Session bearer tokens (issued by 'com.atproto.server.createSession')
-- * DPoP-bound OAuth access tokens
--
-- The verifier populates 'xsrCaller' with the authenticated DID.
module ATProto.PDS.Server.Auth
  ( pdsAuthVerifier
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import           Data.IORef                 (readIORef)
import qualified Data.CaseInsensitive       as CI
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T

import ATProto.XRPC.Server.Types (AuthResult (..), AuthVerifier)
import ATProto.PDS.Server.Env    (Env (..), AppM, SessionInfo (..))

-- | Build an 'AuthVerifier' for the PDS.
--
-- Checks for a @Bearer@ token in the @Authorization@ header and looks
-- it up in the in-memory session store.  Falls back to allowing
-- unauthenticated access (handler decides whether to reject).
pdsAuthVerifier :: AuthVerifier (AppM s) T.Text
pdsAuthVerifier headers = do
  case Map.lookup (CI.mk "authorization") headers of
    Nothing ->
      return (AuthOk Nothing)
    Just val -> do
      let token = extractToken val
      sessRef <- asks envSessions
      sess <- liftIO (readIORef sessRef)
      case Map.lookup token sess of
        Just si ->
          return (AuthOk (Just (siDid si)))
        Nothing ->
          -- Unknown token; report as anonymous rather than failing,
          -- so that public endpoints still work.  Protected endpoints
          -- check 'xsrCaller' themselves.
          return (AuthOk Nothing)

-- | Strip the \"Bearer \" or \"DPoP \" prefix from the Authorization value.
extractToken :: T.Text -> T.Text
extractToken val =
  let stripped = T.strip val
  in case () of
    _ | "Bearer " `T.isPrefixOf` stripped -> T.strip (T.drop 7 stripped)
      | "bearer " `T.isPrefixOf` stripped -> T.strip (T.drop 7 stripped)
      | "DPoP "   `T.isPrefixOf` stripped -> T.strip (T.drop 5 stripped)
      | "dpop "   `T.isPrefixOf` stripped -> T.strip (T.drop 5 stripped)
      | otherwise                         -> stripped
