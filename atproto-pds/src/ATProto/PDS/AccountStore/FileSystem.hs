-- | File-system account store backend.
--
-- Each account's data lives in its own subdirectory under the configured
-- base directory:
--
-- @
-- basedir/
--   session.key          ← 'Web.ClientSession' symmetric key (binary)
--   accounts/
--     \<sanitised-did\>/
--       account          ← key=value text (handle, email, created_at, deactivated)
--       signing_key      ← \<curve\>:\<hex-bytes\>
--       password_hash    ← bcrypt hash, base64-encoded
--       plc_rotation_key ← \<curve\>:\<hex-bytes\>, optional
-- @
--
-- Session tokens are __self-describing__: they are AES-encrypted blobs
-- produced by 'makeSessionToken' (via 'Web.ClientSession') that embed the
-- actor's DID.  No per-session files are written; 'getSession' simply
-- decrypts the token to recover the DID.  The symmetric key is persisted
-- in @basedir\/session.key@ so tokens remain valid across process restarts.
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

import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Char              (intToDigit)
import           Data.Word              (Word8)
import           Numeric                (readHex)
import           System.Directory       (createDirectoryIfMissing,
                                         doesDirectoryExist, doesFileExist,
                                         removeDirectoryRecursive)
import           System.FilePath        ((</>))

import           Web.ClientSession      (Key, getKey)

import           ATProto.Crypto.Types   (Curve (..), PrivKey (..))
import           ATProto.Syntax.DID     (DID, unDID)
import           ATProto.Syntax.Handle  (parseHandle, unHandle)
import           ATProto.PDS.AccountStore

-- ---------------------------------------------------------------------------
-- Store handle
-- ---------------------------------------------------------------------------

-- | A file-system-backed account store rooted at a base directory.
data FileAccountStore = FileAccountStore
  { fasBaseDir    :: FilePath
  , fasSessionKey :: Key
    -- ^ Symmetric key for clientsession tokens, loaded from @basedir\/session.key@.
  }

-- | Create a 'FileAccountStore' rooted at @basedir@.
--
-- Creates @basedir\/accounts\/@ if it does not already exist and loads (or
-- creates) the session key from @basedir\/session.key@.
newFileAccountStore :: FilePath -> IO FileAccountStore
newFileAccountStore basedir = do
  createDirectoryIfMissing True (basedir </> "accounts")
  key <- getKey (basedir </> "session.key")
  return FileAccountStore
    { fasBaseDir    = basedir
    , fasSessionKey = key
    }

-- ---------------------------------------------------------------------------
-- Path helpers
-- ---------------------------------------------------------------------------

accountDir :: FileAccountStore -> DID -> FilePath
accountDir s did = fasBaseDir s </> "accounts" </> sanitiseDID did

-- | Replace @:@ with @_@ to make a DID safe as a directory name.
sanitiseDID :: DID -> FilePath
sanitiseDID = T.unpack . T.map (\c -> if c == ':' then '_' else c) . unDID

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

  -- | Return the session key loaded from @basedir\/session.key@.
  getSessionKey s = return (fasSessionKey s)

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
