-- | Stand-alone AT Protocol PDS server.
--
-- Starts a Warp HTTP server on port 3000 (or @$PORT@) with an
-- in-memory storage backend.
module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import           Network.Wai.Handler.Warp   (run)
import           System.Environment         (lookupEnv)

import           ATProto.Crypto             (generateKeyPair, Curve (..))
import           ATProto.PDS.Storage.InMemory (newInMemoryStore)
import           ATProto.PDS.Server

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = maybe 3000 read portStr :: Int
      hostname = "localhost"
      issuer   = "http://localhost:" <> T.pack (show port)

  store <- newInMemoryStore
  (priv, _pub) <- generateKeyPair P256

  env <- newEnv store priv issuer hostname

  TIO.putStrLn $ "AT Protocol PDS server starting on http://localhost:" <> T.pack (show port)
  run port (pdsApplication env)
