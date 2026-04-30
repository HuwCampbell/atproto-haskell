-- | File-system account store backend.
--
-- Each account's data lives in its own subdirectory under the configured
-- base directory:
--
-- @
-- basedir/
--   jwt.key              ← raw 32-byte HMAC-SHA256 key
--   accounts/
--     \<sanitised-did\>/
--       account          ← key=value text (handle, email, created_at, deactivated)
--       signing_key      ← \<curve\>:\<hex-bytes\>
--       password_hash    ← standard bcrypt hash (e.g. @$2b$10$...@, 60 chars)
--       plc_rotation_key ← \<curve\>:\<hex-bytes\>, optional
--   refresh_tokens/
--     \<jti\>             ← key=value text (did, expires_at)
-- @
--
-- Access and refresh tokens are HS256-signed JWTs.  The 32-byte HMAC key
-- is persisted in @basedir\/jwt.key@ so tokens remain valid across
-- process restarts.  Refresh tokens are stored as individual files under
-- @basedir\/refresh_tokens\/\<jti\>@; revoking a token removes its file.
--
-- Create a store with 'newFileAccountStore':
--
-- @
-- store <- newFileAccountStore "\/var\/lib\/pds"
-- createAccount store account signingKey
-- @
module ATProto.PDS.AccountStore.FileSystem
  ( FileAccountStore
  , newFileAccountStore
  ) where

import           Control.Monad          (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Char              (intToDigit)
import           Data.Word              (Word8)
import           Numeric                (readHex)
import           System.Directory       (createDirectoryIfMissing,
                                         doesDirectoryExist, doesFileExist,
                                         listDirectory,
                                         removeDirectoryRecursive,
                                         removeFile)
import           System.FilePath        ((</>))

import           ATProto.Crypto.Types   (Curve (..), PrivKey (..))
import           ATProto.Syntax.DID     (DID, parseDID, unDID)
import           ATProto.Syntax.Handle  (parseHandle, unHandle)
import           ATProto.PDS.AccountStore

-- ---------------------------------------------------------------------------
-- Store handle
-- ---------------------------------------------------------------------------

-- | A file-system-backed account store rooted at a base directory.
data FileAccountStore = FileAccountStore
  { fasBaseDir :: FilePath
  , fasJwtKey  :: JwtKey
    -- ^ HMAC-SHA256 key loaded from @basedir\/jwt.key@.
  }

-- | Create a 'FileAccountStore' rooted at @basedir@.
--
-- Creates @basedir\/accounts\/@ and @basedir\/refresh_tokens\/@ if they do
-- not already exist and loads (or creates) the JWT key from
-- @basedir\/jwt.key@.
newFileAccountStore :: FilePath -> IO FileAccountStore
newFileAccountStore basedir = do
  createDirectoryIfMissing True (basedir </> "accounts")
  createDirectoryIfMissing True (basedir </> "refresh_tokens")
  key <- loadOrCreateJwtKey (basedir </> "jwt.key")
  return FileAccountStore
    { fasBaseDir = basedir
    , fasJwtKey  = key
    }

-- ---------------------------------------------------------------------------
-- Path helpers
-- ---------------------------------------------------------------------------

accountDir :: FileAccountStore -> DID -> FilePath
accountDir s did = fasBaseDir s </> "accounts" </> sanitiseDID did

refreshTokenDir :: FileAccountStore -> FilePath
refreshTokenDir s = fasBaseDir s </> "refresh_tokens"

refreshTokenPath :: FileAccountStore -> T.Text -> FilePath
refreshTokenPath s jti = refreshTokenDir s </> T.unpack jti

-- | Replace @:@ with @_@ to make a DID safe as a directory name.
sanitiseDID :: DID -> FilePath
sanitiseDID = T.unpack . T.map (\c -> if c == ':' then '_' else c) . unDID

-- ---------------------------------------------------------------------------
-- JWT key file helpers
-- ---------------------------------------------------------------------------

-- | Load the JWT key from @path@, or generate and persist a fresh one if
-- the file does not exist.
loadOrCreateJwtKey :: FilePath -> IO JwtKey
loadOrCreateJwtKey path = do
  exists <- doesFileExist path
  if exists
    then JwtKey <$> BS.readFile path
    else do
      key@(JwtKey bs) <- generateJwtKey
      BS.writeFile path bs
      return key

-- ---------------------------------------------------------------------------
-- AccountStore instance
-- ---------------------------------------------------------------------------

instance AccountStore FileAccountStore where
  createAccount s acc key = liftIO $ do
    let dir = accountDir s (accountDid acc)
    createDirectoryIfMissing True dir
    writeFile (dir </> "account")     (encodeAccount acc)
    writeFile (dir </> "signing_key") (encodeKey key)

  getAccount s did = liftIO $ do
    let path = accountDir s did </> "account"
    exists <- doesFileExist path
    if not exists
      then return Nothing
      else do
        content <- readFile path
        return (decodeAccount did content)

  updateAccount s acc = liftIO $ do
    let dir = accountDir s (accountDid acc)
    exists <- doesDirectoryExist dir
    when exists $
      writeFile (dir </> "account") (encodeAccount acc)

  deleteAccount s did = liftIO $ do
    let dir = accountDir s did
    exists <- doesDirectoryExist dir
    when exists $ removeDirectoryRecursive dir

  storePassword s did h = liftIO $ do
    let dir = accountDir s did
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "password_hash") (unPasswordHash h)

  getPasswordHash s did = liftIO $ do
    let path = accountDir s did </> "password_hash"
    exists <- doesFileExist path
    if not exists
      then return Nothing
      else Just . PasswordHash <$> TIO.readFile path

  getSigningKey s did = liftIO $ do
    let path = accountDir s did </> "signing_key"
    exists <- doesFileExist path
    if not exists
      then return Nothing
      else decodeKey <$> readFile path

  getJwtKey s = return (fasJwtKey s)

  storeRefreshToken s rec = liftIO $ do
    let path = refreshTokenPath s (rtrJti rec)
    writeFile path (encodeRefreshToken rec)

  getRefreshToken s jti = liftIO $ do
    let path = refreshTokenPath s jti
    exists <- doesFileExist path
    if not exists
      then return Nothing
      else decodeRefreshToken jti <$> readFile path

  revokeRefreshToken s jti = liftIO $ do
    let path = refreshTokenPath s jti
    exists <- doesFileExist path
    when exists $ removeFile path

  revokeRefreshTokensByDid s did = liftIO $ do
    let dir = refreshTokenDir s
    files <- listDirectory dir
    forM_ files $ \file -> do
      let path = dir </> file
          jti  = T.pack file
      content <- readFile path
      case decodeRefreshToken jti content of
        Just rec | rtrDid rec == did -> removeFile path
        _                            -> return ()

  storePlcRotationKey s did key = liftIO $ do
    let dir = accountDir s did
    createDirectoryIfMissing True dir
    writeFile (dir </> "plc_rotation_key") (encodeKey key)

  getPlcRotationKey s did = liftIO $ do
    let path = accountDir s did </> "plc_rotation_key"
    exists <- doesFileExist path
    if not exists
      then return Nothing
      else decodeKey <$> readFile path

-- ---------------------------------------------------------------------------
-- Account key=value encoding
-- ---------------------------------------------------------------------------

-- | Encode account metadata as a @key=value@ text file (one pair per line).
encodeAccount :: Account -> String
encodeAccount acc = unlines
  [ "handle="      ++ T.unpack (unHandle (accountHandle acc))
  , "email="       ++ maybe "" T.unpack (accountEmail acc)
  , "created_at="  ++ show (accountCreatedAt acc)
  , "deactivated=" ++ (if accountDeactivated acc then "true" else "false")
  ]

-- | Decode account metadata from @key=value@ text.
decodeAccount :: DID -> String -> Maybe Account
decodeAccount did content = do
  let kvs = parseKVs content
  handleStr      <- lookup "handle" kvs
  handle         <- either (const Nothing) Just (parseHandle (T.pack handleStr))
  let emailStr    = lookup "email" kvs
      email       = emailStr >>= \e -> if null e then Nothing else Just (T.pack e)
  createdAtStr   <- lookup "created_at" kvs
  createdAt      <- case [ v | (v, "") <- reads createdAtStr ] of
                      (v:_) -> Just v
                      []    -> Nothing
  deactivatedStr <- lookup "deactivated" kvs
  deactivated    <- case deactivatedStr of
                      "true"  -> Just True
                      "false" -> Just False
                      _       -> Nothing
  return Account
    { accountDid         = did
    , accountHandle      = handle
    , accountEmail       = email
    , accountCreatedAt   = createdAt
    , accountDeactivated = deactivated
    }

-- ---------------------------------------------------------------------------
-- Refresh token key=value encoding
-- ---------------------------------------------------------------------------

-- | Encode a 'RefreshTokenRecord' as @key=value@ text.
encodeRefreshToken :: RefreshTokenRecord -> String
encodeRefreshToken rec = unlines
  [ "did="        ++ T.unpack (unDID (rtrDid rec))
  , "expires_at=" ++ show (rtrExpiresAt rec)
  ]

-- | Decode a 'RefreshTokenRecord' from @key=value@ text.
decodeRefreshToken :: T.Text -> String -> Maybe RefreshTokenRecord
decodeRefreshToken jti content = do
  let kvs = parseKVs content
  didStr    <- lookup "did" kvs
  did       <- either (const Nothing) Just (parseDID (T.pack didStr))
  expStr    <- lookup "expires_at" kvs
  expiresAt <- case [ v | (v, "") <- reads expStr ] of
                 (v:_) -> Just v
                 []    -> Nothing
  return RefreshTokenRecord
    { rtrJti       = jti
    , rtrDid       = did
    , rtrExpiresAt = expiresAt
    }

-- | Parse @key=value@ lines, skipping blank lines and @#@ comments.
parseKVs :: String -> [(String, String)]
parseKVs content =
  [ (k, v)
  | line <- lines content
  , not (null line)
  , case line of { '#':_ -> False; _ -> True }
  , let (k, rest) = break (== '=') line
  , not (null rest)
  , let v = drop 1 rest
  ]

-- ---------------------------------------------------------------------------
-- Private key encoding  (<curve>:<hex-bytes>)
-- ---------------------------------------------------------------------------

-- | Encode a 'PrivKey' as @\<curve\>:\<hex-bytes\>@.
encodeKey :: PrivKey -> String
encodeKey (PrivKey curve bytes) =
  curveTag curve ++ ":" ++ bytesToHex (BS.unpack bytes)
  where
    curveTag P256      = "p256"
    curveTag Secp256k1 = "secp256k1"

-- | Decode a 'PrivKey' from @\<curve\>:\<hex-bytes\>@.
decodeKey :: String -> Maybe PrivKey
decodeKey s =
  case break (== ':') s of
    (curveStr, ':' : hexStr) ->
      case (parseCurve curveStr, hexToBytes (filter (/= '\n') hexStr)) of
        (Just curve, Just bytes) -> Just (PrivKey curve bytes)
        _                        -> Nothing
    _ -> Nothing
  where
    parseCurve "p256"      = Just P256
    parseCurve "secp256k1" = Just Secp256k1
    parseCurve _           = Nothing

-- | Encode bytes as lowercase hex.
bytesToHex :: [Word8] -> String
bytesToHex = concatMap (\b -> [intToDigit (fromIntegral b `div` 16),
                                intToDigit (fromIntegral b `mod` 16)])

-- | Decode lowercase hex to bytes.
hexToBytes :: String -> Maybe BS.ByteString
hexToBytes s
  | odd (length s) = Nothing
  | otherwise      = fmap BS.pack (go s)
  where
    go []             = Just []
    go (h1 : h2 : t) = case readHex [h1, h2] of
                          [(w, "")] -> fmap (w :) (go t)
                          _         -> Nothing
    go _              = Nothing

