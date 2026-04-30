-- | Tests for the account store backends (in-memory and file-system).
module Test.ATProto.PDS.AccountStore (tests) where

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Data.Time.Clock            (getCurrentTime)
import qualified Data.Text                  as T
import           System.Directory           (removeDirectoryRecursive,
                                             doesDirectoryExist,
                                             getTemporaryDirectory)
import           System.FilePath            ((</>))

import           ATProto.Crypto.EC          (generateKeyPair)
import           ATProto.Crypto.Types       (Curve (..))
import           ATProto.Syntax.DID         (DID, parseDID)
import           ATProto.Syntax.Handle      (Handle, parseHandle)

import           ATProto.PDS.AccountStore
import           ATProto.PDS.AccountStore.InMemory   (newInMemoryAccountStore)
import           ATProto.PDS.AccountStore.FileSystem (FileAccountStore,
                                                      newFileAccountStore)

-- ---------------------------------------------------------------------------
-- Shared fixtures
-- ---------------------------------------------------------------------------

testDID1 :: DID
testDID1 = case parseDID "did:plc:acctest1" of { Right d -> d; Left e -> error e }

testHandle1 :: Handle
testHandle1 = case parseHandle "alice.example.com" of { Right h -> h; Left e -> error e }

testAud :: T.Text
testAud = "did:plc:pds-service"

makeAccount :: DID -> IO Account
makeAccount did = do
  now <- getCurrentTime
  return Account
    { accountDid         = did
    , accountHandle      = testHandle1
    , accountEmail       = Just "alice@example.com"
    , accountCreatedAt   = now
    , accountDeactivated = False
    }

-- ---------------------------------------------------------------------------
-- Generic properties parametrised over a store
-- ---------------------------------------------------------------------------

-- | Create an account and retrieve it by DID.
prop_createGetAccount :: AccountStore s => IO s -> Property
prop_createGetAccount mkStore = withTests 5 . property $ do
  s   <- evalIO mkStore
  acc <- evalIO (makeAccount testDID1)
  (key, _) <- evalIO (generateKeyPair P256)
  evalIO (createAccount s acc key)
  result <- evalIO (getAccount s testDID1)
  result === Just acc

-- | Unknown DIDs return Nothing.
prop_getUnknownAccount :: AccountStore s => IO s -> Property
prop_getUnknownAccount mkStore = withTests 5 . property $ do
  s      <- evalIO mkStore
  result <- evalIO (getAccount s testDID1)
  result === Nothing

-- | Delete removes the account.
prop_deleteAccount :: AccountStore s => IO s -> Property
prop_deleteAccount mkStore = withTests 5 . property $ do
  s   <- evalIO mkStore
  acc <- evalIO (makeAccount testDID1)
  (key, _) <- evalIO (generateKeyPair P256)
  evalIO (createAccount s acc key)
  evalIO (deleteAccount s testDID1)
  result <- evalIO (getAccount s testDID1)
  result === Nothing

-- | Store and retrieve a password hash.
prop_passwordRoundTrip :: AccountStore s => IO s -> Property
prop_passwordRoundTrip mkStore = withTests 5 . property $ do
  s   <- evalIO mkStore
  acc <- evalIO (makeAccount testDID1)
  (key, _) <- evalIO (generateKeyPair P256)
  evalIO (createAccount s acc key)
  h <- evalIO (hashPassword "s3cret!")
  evalIO (storePassword s testDID1 h)
  result <- evalIO (getPasswordHash s testDID1)
  result === Just h

-- | checkPassword succeeds for the correct password.
prop_checkPasswordCorrect :: Property
prop_checkPasswordCorrect = withTests 10 . property $ do
  pwd <- forAll (Gen.text (Range.linear 8 30) Gen.unicodeAll)
  h   <- evalIO (hashPassword pwd)
  assert (checkPassword pwd h)

-- | checkPassword fails for a wrong password.
prop_checkPasswordWrong :: Property
prop_checkPasswordWrong = withTests 10 . property $ do
  pwd  <- forAll (Gen.text (Range.linear 8 30) Gen.unicodeAll)
  bad  <- forAll (Gen.text (Range.linear 8 30) Gen.unicodeAll)
  h    <- evalIO (hashPassword pwd)
  if pwd == bad
    then success   -- skip coincidental equality
    else assert (not (checkPassword bad h))

-- | createAccessToken + verifyAccessToken returns the correct DID.
prop_accessTokenRoundTrip :: AccountStore s => IO s -> Property
prop_accessTokenRoundTrip mkStore = withTests 5 . property $ do
  s   <- evalIO mkStore
  key <- evalIO (getJwtKey s)
  tok <- evalIO (createAccessToken key testAud testDID1)
  result <- evalIO (verifyAccessToken key tok)
  result === Right testDID1

-- | createRefreshToken + verifyRefreshToken returns the correct DID and jti.
prop_refreshTokenRoundTrip :: AccountStore s => IO s -> Property
prop_refreshTokenRoundTrip mkStore = withTests 5 . property $ do
  s   <- evalIO mkStore
  key <- evalIO (getJwtKey s)
  (tok, rec) <- evalIO (createRefreshToken key testAud testDID1)
  result <- evalIO (verifyRefreshToken key tok)
  result === Right (testDID1, rtrJti rec)

-- | Storing and looking up a refresh token by jti works.
prop_storeGetRefreshToken :: AccountStore s => IO s -> Property
prop_storeGetRefreshToken mkStore = withTests 5 . property $ do
  s   <- evalIO mkStore
  key <- evalIO (getJwtKey s)
  (_, rec) <- evalIO (createRefreshToken key testAud testDID1)
  evalIO (storeRefreshToken s rec)
  result <- evalIO (getRefreshToken s (rtrJti rec))
  result === Just rec

-- | Revoking a refresh token by jti removes it.
prop_revokeRefreshToken :: AccountStore s => IO s -> Property
prop_revokeRefreshToken mkStore = withTests 5 . property $ do
  s   <- evalIO mkStore
  key <- evalIO (getJwtKey s)
  (_, rec) <- evalIO (createRefreshToken key testAud testDID1)
  evalIO (storeRefreshToken s rec)
  evalIO (revokeRefreshToken s (rtrJti rec))
  result <- evalIO (getRefreshToken s (rtrJti rec))
  result === Nothing

-- | Revoking all tokens for a DID removes them.
prop_revokeRefreshTokensByDid :: AccountStore s => IO s -> Property
prop_revokeRefreshTokensByDid mkStore = withTests 5 . property $ do
  s   <- evalIO mkStore
  key <- evalIO (getJwtKey s)
  (_, rec1) <- evalIO (createRefreshToken key testAud testDID1)
  (_, rec2) <- evalIO (createRefreshToken key testAud testDID1)
  evalIO (storeRefreshToken s rec1)
  evalIO (storeRefreshToken s rec2)
  evalIO (revokeRefreshTokensByDid s testDID1)
  r1 <- evalIO (getRefreshToken s (rtrJti rec1))
  r2 <- evalIO (getRefreshToken s (rtrJti rec2))
  r1 === Nothing
  r2 === Nothing

-- | A garbage string is rejected by verifyAccessToken.
prop_invalidAccessTokenRejected :: AccountStore s => IO s -> Property
prop_invalidAccessTokenRejected mkStore = withTests 5 . property $ do
  s   <- evalIO mkStore
  key <- evalIO (getJwtKey s)
  bad <- forAll (Gen.text (Range.linear 10 50) Gen.alphaNum)
  result <- evalIO (verifyAccessToken key bad)
  assert (isLeft result)

-- | Store and retrieve a PLC rotation key.
prop_plcRotationKeyRoundTrip :: AccountStore s => IO s -> Property
prop_plcRotationKeyRoundTrip mkStore = withTests 5 . property $ do
  s <- evalIO mkStore
  acc <- evalIO (makeAccount testDID1)
  (sigKey, _) <- evalIO (generateKeyPair P256)
  (rotKey, _) <- evalIO (generateKeyPair Secp256k1)
  evalIO (createAccount s acc sigKey)
  evalIO (storePlcRotationKey s testDID1 rotKey)
  result <- evalIO (getPlcRotationKey s testDID1)
  result === Just rotKey

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

-- ---------------------------------------------------------------------------
-- In-memory backend
-- ---------------------------------------------------------------------------

tests_inMemory :: [(PropertyName, Property)]
tests_inMemory =
  [ ("in-memory: create/get",               prop_createGetAccount           (newInMemoryAccountStore))
  , ("in-memory: get unknown",              prop_getUnknownAccount          (newInMemoryAccountStore))
  , ("in-memory: delete",                   prop_deleteAccount              (newInMemoryAccountStore))
  , ("in-memory: password round-trip",      prop_passwordRoundTrip          (newInMemoryAccountStore))
  , ("in-memory: access token round-trip",  prop_accessTokenRoundTrip       (newInMemoryAccountStore))
  , ("in-memory: refresh token round-trip", prop_refreshTokenRoundTrip      (newInMemoryAccountStore))
  , ("in-memory: store/get refresh token",  prop_storeGetRefreshToken       (newInMemoryAccountStore))
  , ("in-memory: revoke refresh token",     prop_revokeRefreshToken         (newInMemoryAccountStore))
  , ("in-memory: revoke tokens by DID",     prop_revokeRefreshTokensByDid   (newInMemoryAccountStore))
  , ("in-memory: invalid access token",     prop_invalidAccessTokenRejected (newInMemoryAccountStore))
  , ("in-memory: PLC rotation key",         prop_plcRotationKeyRoundTrip    (newInMemoryAccountStore))
  ]

-- ---------------------------------------------------------------------------
-- File-system backend
-- ---------------------------------------------------------------------------

setupFileAccountStore :: IO FileAccountStore
setupFileAccountStore = do
  tmpDir <- getTemporaryDirectory
  let testDir = tmpDir </> "atproto-pds-test-accountstore"
  exists <- doesDirectoryExist testDir
  if exists then removeDirectoryRecursive testDir else return ()
  newFileAccountStore testDir

tests_fileSystem :: [(PropertyName, Property)]
tests_fileSystem =
  [ ("file: create/get",               prop_createGetAccount           setupFileAccountStore)
  , ("file: get unknown",              prop_getUnknownAccount          setupFileAccountStore)
  , ("file: delete",                   prop_deleteAccount              setupFileAccountStore)
  , ("file: password round-trip",      prop_passwordRoundTrip          setupFileAccountStore)
  , ("file: access token round-trip",  prop_accessTokenRoundTrip       setupFileAccountStore)
  , ("file: refresh token round-trip", prop_refreshTokenRoundTrip      setupFileAccountStore)
  , ("file: store/get refresh token",  prop_storeGetRefreshToken       setupFileAccountStore)
  , ("file: revoke refresh token",     prop_revokeRefreshToken         setupFileAccountStore)
  , ("file: revoke tokens by DID",     prop_revokeRefreshTokensByDid   setupFileAccountStore)
  , ("file: invalid access token",     prop_invalidAccessTokenRejected setupFileAccountStore)
  , ("file: PLC rotation key",         prop_plcRotationKeyRoundTrip    setupFileAccountStore)
  ]

-- ---------------------------------------------------------------------------
-- Password helpers (not backend-specific)
-- ---------------------------------------------------------------------------

tests_password :: [(PropertyName, Property)]
tests_password =
  [ ("password: correct password accepted", prop_checkPasswordCorrect)
  , ("password: wrong password rejected",   prop_checkPasswordWrong)
  ]

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.PDS.AccountStore"
  (tests_inMemory ++ tests_fileSystem ++ tests_password)
