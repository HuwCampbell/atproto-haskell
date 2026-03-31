-- | Tests for file-system storage backend.
module Test.ATProto.PDS.Storage (tests) where

import Hedgehog hiding (Update)
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import qualified Data.ByteString      as BS
import qualified Data.Text            as T

import System.Directory               (removeDirectoryRecursive, doesDirectoryExist,
                                       getTemporaryDirectory)
import System.FilePath                ((</>))

import ATProto.Car.Cid                (CidBytes, unsafeRawCid)
import ATProto.Crypto.EC              (generateKeyPair)
import ATProto.Crypto.Types           (Curve (..), PrivKey)
import ATProto.Syntax.DID             (DID, parseDID)
import ATProto.PDS.Storage            (BlockStore (..), RepoStore (..))
import ATProto.PDS.Storage.FileSystem (FileStore, newFileStore)
import ATProto.PDS.Repo

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genRecordBytes :: Gen BS.ByteString
genRecordBytes = Gen.bytes (Range.linear 1 100)

genRecordKey :: Gen T.Text
genRecordKey = Gen.text (Range.linear 1 15) (Gen.element (['a'..'z'] ++ ['0'..'9']))

-- ---------------------------------------------------------------------------
-- Test DID & temp dir
-- ---------------------------------------------------------------------------

testDID :: DID
testDID = case parseDID "did:plc:filetest" of
  Right d -> d
  Left  e -> error ("testDID: " ++ e)

setupFileStore :: IO (FileStore, PrivKey)
setupFileStore = do
  tmpDir <- getTemporaryDirectory
  let testDir = tmpDir </> "atproto-pds-test-filestore"
  exists <- doesDirectoryExist testDir
  if exists then removeDirectoryRecursive testDir else return ()
  store <- newFileStore testDir
  (priv, _pub) <- generateKeyPair P256
  return (store, priv)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Block store round-trip: put then get returns the same bytes.
prop_blockStoreRoundTrip :: Property
prop_blockStoreRoundTrip = withTests 5 . property $ do
  body <- forAll genRecordBytes
  (store, _) <- evalIO setupFileStore
  let cid = unsafeRawCid (BS.pack [0x01, 0x71, 0x12, 0x20] <> BS.replicate 32 0x42)
  evalIO (putBlock store cid body)
  result <- evalIO (getBlock store cid)
  result === Just body

-- | Repo head round-trip: set then get returns the same CID.
prop_repoHeadRoundTrip :: Property
prop_repoHeadRoundTrip = withTests 5 . property $ do
  (store, _) <- evalIO setupFileStore
  let cid = unsafeRawCid (BS.pack [0x01, 0x71, 0x12, 0x20] <> BS.replicate 32 0xAB)
  -- Initially, no head.
  initial <- evalIO (getRepoHead store testDID)
  initial === Nothing
  -- After setting, it should be the CID we set.
  evalIO (setRepoHead store testDID cid)
  result <- evalIO (getRepoHead store testDID)
  result === Just cid

-- | File-backed create/get round-trip.
prop_fileStoreCreateGet :: Property
prop_fileStoreCreateGet = withTests 10 . property $ do
  body <- forAll genRecordBytes
  rk   <- forAll genRecordKey
  (store, key) <- evalIO setupFileStore
  _ <- evalEither =<< evalIO (initRepo store testDID key)
  commitCid <- evalEither =<< evalIO (createRecord store testDID key "com.test.record" rk body)
  result <- evalEither =<< evalIO (getRecord store commitCid "com.test.record" rk)
  result === Just body

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.PDS.Storage"
  [ ("file block store round-trip",  prop_blockStoreRoundTrip)
  , ("file repo head round-trip",    prop_repoHeadRoundTrip)
  , ("file store create/get",        prop_fileStoreCreateGet)
  ]
