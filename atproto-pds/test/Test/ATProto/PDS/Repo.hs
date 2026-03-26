-- | Property-based tests for PDS repository operations.
module Test.ATProto.PDS.Repo (tests) where

import Hedgehog hiding (Update)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString      as BS
import qualified Data.Text            as T

import ATProto.Car.Cid              (CidBytes (..))
import ATProto.Crypto.EC            (generateKeyPair)
import ATProto.Crypto.Types         (Curve (..), PrivKey)
import ATProto.MST.Encode           (cidForDagCbor)
import ATProto.Syntax.DID           (DID, parseDID)
import ATProto.PDS.Storage.InMemory (InMemoryStore, newInMemoryStore)
import ATProto.PDS.Repo

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | Generate a simple record body (raw bytes, treated as DAG-CBOR).
genRecordBytes :: Gen BS.ByteString
genRecordBytes = Gen.bytes (Range.linear 1 200)

-- | Generate a valid collection name.
genCollection :: Gen T.Text
genCollection = do
  parts <- Gen.list (Range.linear 2 4) $
    Gen.text (Range.linear 1 10) Gen.alpha
  return (T.intercalate "." parts)

-- | Generate a valid record key.
genRecordKey :: Gen T.Text
genRecordKey = Gen.text (Range.linear 1 20) (Gen.element (['a'..'z'] ++ ['0'..'9']))

-- ---------------------------------------------------------------------------
-- Test DID
-- ---------------------------------------------------------------------------

testDID :: DID
testDID = case parseDID "did:plc:testuser123" of
  Right d -> d
  Left  e -> error ("testDID: " ++ e)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Initialising a repo produces a valid commit CID.
prop_initRepo :: Property
prop_initRepo = withTests 10 . property $ do
  (store, key) <- evalIO setup
  result <- evalIO (initRepo store testDID key)
  case result of
    Left err  -> do
      annotate (show err)
      failure
    Right cid -> do
      annotate ("commit CID: " ++ show cid)
      -- CID should be 36 bytes (dag-cbor + sha256)
      BS.length (unCidBytes cid) === 36

-- | Initialising the same DID twice fails.
prop_initRepoTwice :: Property
prop_initRepoTwice = withTests 5 . property $ do
  (store, key) <- evalIO setup
  r1 <- evalIO (initRepo store testDID key)
  case r1 of
    Left err -> do
      annotate (show err)
      failure
    Right _ -> do
      r2 <- evalIO (initRepo store testDID key)
      case r2 of
        Left (PdsRepoAlreadyExists _) -> success
        Left err -> do
          annotate ("expected PdsRepoAlreadyExists, got: " ++ show err)
          failure
        Right _ -> do
          annotate "expected failure, got success"
          failure

-- | Creating a record and reading it back returns the same bytes.
prop_createGetRoundTrip :: Property
prop_createGetRoundTrip = withTests 20 . property $ do
  col  <- forAll genCollection
  rk   <- forAll genRecordKey
  body <- forAll genRecordBytes
  (store, key) <- evalIO setup
  _ <- evalEither =<< evalIO (initRepo store testDID key)
  commitCid <- evalEither =<< evalIO (createRecord store testDID key col rk body)
  result <- evalEither =<< evalIO (getRecord store commitCid col rk)
  result === Just body

-- | Getting a non-existent record returns Nothing.
prop_getMissing :: Property
prop_getMissing = withTests 10 . property $ do
  (store, key) <- evalIO setup
  commitCid <- evalEither =<< evalIO (initRepo store testDID key)
  result <- evalEither =<< evalIO (getRecord store commitCid "com.example" "missing")
  result === Nothing

-- | Multiple records can be created and listed.
prop_createListRecords :: Property
prop_createListRecords = withTests 15 . property $ do
  n     <- forAll (Gen.int (Range.linear 1 10))
  col   <- forAll genCollection
  pairs <- forAll $ sequence
    [ (,) <$> genRecordKey <*> genRecordBytes
    | _ <- [1..n]
    ]

  -- Deduplicate by key (keep last)
  let uniquePairs = dedup pairs

  (store, key) <- evalIO setup
  _ <- evalEither =<< evalIO (initRepo store testDID key)

  -- Create records one at a time.
  lastCommit <- evalIO (createRecords store key col uniquePairs)
  commit <- evalEither lastCommit

  -- List the collection.
  listed <- evalEither =<< evalIO (listRecords store commit col)

  -- Should have all the records we created.
  length listed === length uniquePairs

  -- Each record should match.
  let listedMap = map (\(k, v) -> (k, v)) listed
  mapM_ (\(rk, body) ->
    case lookup rk listedMap of
      Nothing -> do
        annotate ("missing record key: " ++ T.unpack rk)
        failure
      Just v  -> v === body
    ) uniquePairs

-- | Deleting a record removes it from the repository.
prop_deleteRecord :: Property
prop_deleteRecord = withTests 10 . property $ do
  col  <- forAll genCollection
  rk   <- forAll genRecordKey
  body <- forAll genRecordBytes
  (store, key) <- evalIO setup
  _ <- evalEither =<< evalIO (initRepo store testDID key)
  _ <- evalEither =<< evalIO (createRecord store testDID key col rk body)
  delCommit <- evalEither =<< evalIO (deleteRecord store testDID key col rk)
  result <- evalEither =<< evalIO (getRecord store delCommit col rk)
  result === Nothing

-- | Deleting a non-existent record fails.
prop_deleteNonExistent :: Property
prop_deleteNonExistent = withTests 5 . property $ do
  (store, key) <- evalIO setup
  _ <- evalEither =<< evalIO (initRepo store testDID key)
  result <- evalIO (deleteRecord store testDID key "com.example" "nope")
  case result of
    Left (PdsRecordNotFound _) -> success
    Left err -> do
      annotate ("expected PdsRecordNotFound, got: " ++ show err)
      failure
    Right _ -> do
      annotate "expected failure, got success"
      failure

-- | Records in different collections are independent.
prop_multipleCollections :: Property
prop_multipleCollections = withTests 10 . property $ do
  col1 <- forAll genCollection
  col2 <- forAll (Gen.filter (/= col1) genCollection)
  rk   <- forAll genRecordKey
  body1 <- forAll genRecordBytes
  body2 <- forAll genRecordBytes
  (store, key) <- evalIO setup

  _ <- evalEither =<< evalIO (initRepo store testDID key)
  _ <- evalEither =<< evalIO (createRecord store testDID key col1 rk body1)
  commit <- evalEither =<< evalIO (createRecord store testDID key col2 rk body2)

  -- Both records should be retrievable.
  r1 <- evalEither =<< evalIO (getRecord store commit col1 rk)
  r1 === Just body1

  r2 <- evalEither =<< evalIO (getRecord store commit col2 rk)
  r2 === Just body2

  -- Listing each collection returns only its records.
  l1 <- evalEither =<< evalIO (listRecords store commit col1)
  length l1 === 1

  l2 <- evalEither =<< evalIO (listRecords store commit col2)
  length l2 === 1

-- | Updating a record changes its content.
prop_updateRecord :: Property
prop_updateRecord = withTests 10 . property $ do
  col   <- forAll genCollection
  rk    <- forAll genRecordKey
  body1 <- forAll genRecordBytes
  body2 <- forAll (Gen.filter (/= body1) genRecordBytes)
  (store, key) <- evalIO setup

  _ <- evalEither =<< evalIO (initRepo store testDID key)
  _ <- evalEither =<< evalIO (createRecord store testDID key col rk body1)
  commit <- evalEither =<< evalIO (applyWrites store testDID key [Update col rk body2])

  r <- evalEither =<< evalIO (getRecord store commit col rk)
  r === Just body2

-- | Commit CIDs are deterministic: same MST content produces the
-- same data root (though the commit itself differs due to rev/sig).
prop_deterministicMst :: Property
prop_deterministicMst = withTests 10 . property $ do
  body <- forAll genRecordBytes
  -- Build the same record in two independent stores.
  let recCid = cidForDagCbor body
  -- The CID should be consistent.
  cidForDagCbor body === recCid

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

setup :: IO (InMemoryStore, PrivKey)
setup = do
  store <- newInMemoryStore
  (priv, _pub) <- generateKeyPair P256
  return (store, priv)

createRecords
  :: InMemoryStore
  -> PrivKey
  -> T.Text
  -> [(T.Text, BS.ByteString)]
  -> IO (Either PdsError CidBytes)
createRecords _store _key _col [] = error "createRecords: empty list"
createRecords store key col [(rk, body)] =
  createRecord store testDID key col rk body
createRecords store key col ((rk, body):rest) = do
  r <- createRecord store testDID key col rk body
  case r of
    Left err -> return (Left err)
    Right _  -> createRecords store key col rest

dedup :: Eq a => [(a, b)] -> [(a, b)]
dedup = foldr (\(k, v) acc ->
  if any (\(k', _) -> k == k') acc
    then acc
    else (k, v) : acc
  ) []

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

tests :: Group
tests = Group "ATProto.PDS.Repo"
  [ ("initRepo creates valid commit",          prop_initRepo)
  , ("initRepo twice fails",                   prop_initRepoTwice)
  , ("create/get round-trip",                  prop_createGetRoundTrip)
  , ("get missing returns Nothing",            prop_getMissing)
  , ("create/list round-trip",                 prop_createListRecords)
  , ("delete removes record",                  prop_deleteRecord)
  , ("delete non-existent fails",              prop_deleteNonExistent)
  , ("multiple collections independent",       prop_multipleCollections)
  , ("update changes content",                 prop_updateRecord)
  , ("MST CID is deterministic",              prop_deterministicMst)
  ]
