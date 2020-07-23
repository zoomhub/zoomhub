{-# LANGUAGE OverloadedStrings #-}
--
-- MinIO Haskell SDK, (C) 2017-2019 MinIO, Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

import qualified Test.QuickCheck                       as Q
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck                 as QC

import           Conduit                               (replicateC)
import qualified Conduit                               as C
import qualified Control.Monad.Trans.Resource          as R
import qualified Data.ByteString                       as BS
import           Data.Conduit                          (yield)
import qualified Data.Conduit.Binary                   as CB
import           Data.Conduit.Combinators              (sinkList)
import qualified Data.HashMap.Strict                   as H
import qualified Data.Text                             as T
import           Data.Time                             (fromGregorian)
import qualified Data.Time                             as Time
import qualified Network.HTTP.Client.MultipartFormData as Form
import qualified Network.HTTP.Conduit                  as NC
import qualified Network.HTTP.Types                    as HT
import           System.Directory                      (getTemporaryDirectory)
import           System.Environment                    (lookupEnv)
import qualified System.IO                             as SIO

import           Lib.Prelude

import           Network.Minio
import           Network.Minio.Data
import           Network.Minio.Data.Crypto
import           Network.Minio.PutObject
import           Network.Minio.S3API
import           Network.Minio.Utils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [liveServerUnitTests]

-- conduit that generates random binary stream of given length
randomDataSrc :: MonadIO m => Int64 -> C.ConduitM () ByteString m ()
randomDataSrc s' = genBS s'
  where
    concatIt bs n = BS.concat $ replicate (fromIntegral q) bs ++
                    [BS.take (fromIntegral r) bs]
      where (q, r) = n `divMod` fromIntegral (BS.length bs)

    genBS s = do
      w8s <- liftIO $ generate $ Q.vectorOf 64 (Q.choose (0, 255))
      let byteArr64 = BS.pack w8s
      if s < oneMiB
        then yield $ concatIt byteArr64 s
        else do yield $ concatIt byteArr64 oneMiB
                genBS (s - oneMiB)

mkRandFile :: R.MonadResource m => Int64 -> m FilePath
mkRandFile size = do
  dir <- liftIO $ getTemporaryDirectory
  C.runConduit $ randomDataSrc size C..| CB.sinkTempFile dir "miniohstest.random"

funTestBucketPrefix :: Text
funTestBucketPrefix = "miniohstest-"

loadTestServer :: IO ConnectInfo
loadTestServer = do
    val <- lookupEnv "MINIO_LOCAL"
    isSecure <- lookupEnv "MINIO_SECURE"
    return $ case (val, isSecure) of
      (Just _, Just _) -> setCreds (Credentials "minio" "minio123") "https://localhost:9000"
      (Just _, Nothing) -> setCreds (Credentials "minio" "minio123") "http://localhost:9000"
      (Nothing, _) -> minioPlayCI

funTestWithBucket :: TestName
                  -> (([Char] -> Minio ()) -> Bucket -> Minio ()) -> TestTree
funTestWithBucket t minioTest = testCaseSteps t $ \step -> do
  -- generate a random name for the bucket
  bktSuffix <- liftIO $ generate $ Q.vectorOf 10 (Q.choose ('a', 'z'))
  let b = T.concat [funTestBucketPrefix, T.pack bktSuffix]
      liftStep = liftIO . step
  connInfo <- loadTestServer
  ret <- runMinio connInfo $ do
    liftStep $ "Creating bucket for test - " ++ t
    foundBucket <- bucketExists b
    liftIO $ foundBucket @?= False
    makeBucket b Nothing
    minioTest liftStep b
    deleteBucket b
  isRight ret @? ("Functional test " ++ t ++ " failed => " ++ show ret)

liveServerUnitTests :: TestTree
liveServerUnitTests = testGroup "Unit tests against a live server"
  [ basicTests
  , listingTest
  , highLevelListingTest
  , lowLevelMultipartTest
  , putObjectSizeTest
  , putObjectNoSizeTest
  , multipartTest
  , putObjectContentTypeTest
  , putObjectContentLanguageTest
  , putObjectStorageClassTest
  , putObjectUserMetadataTest
  , getObjectTest
  , copyObjectTests
  , presignedUrlFunTest
  , presignedPostPolicyFunTest
  , bucketPolicyFunTest
  , getNPutSSECTest
  ]

basicTests :: TestTree
basicTests = funTestWithBucket "Basic tests" $
  \step bucket -> do
    step "getService works and contains the test bucket."
    buckets <- getService
    unless (length (filter (== bucket) $ map biName buckets) == 1) $
      liftIO $
      assertFailure ("The bucket " ++ show bucket ++
                     " was expected to exist.")

    step "makeBucket again to check if BucketAlreadyOwnedByYou exception is raised."
    mbE <- try $ makeBucket bucket Nothing
    case mbE of
      Left exn -> liftIO $ exn @?= BucketAlreadyOwnedByYou
      _        -> return ()

    step "makeBucket with an invalid bucket name and check for appropriate exception."
    invalidMBE <- try $ makeBucket "invalidBucketName" Nothing
    case invalidMBE of
      Left exn -> liftIO $ exn @?= MErrVInvalidBucketName "invalidBucketName"
      _        -> return ()

    step "getLocation works"
    region <- getLocation bucket
    liftIO $ region == "us-east-1" @? ("Got unexpected region => " ++ show region)

    step "singlepart putObject works"
    fPutObject bucket "lsb-release" "/etc/lsb-release" defaultPutObjectOptions

    step "fPutObject onto a non-existent bucket and check for NoSuchBucket exception"
    fpE <- try $ fPutObject "nosuchbucket" "lsb-release" "/etc/lsb-release" defaultPutObjectOptions
    case fpE of
      Left exn -> liftIO $ exn @?= NoSuchBucket
      _        -> return ()

    outFile <- mkRandFile 0
    step "simple fGetObject works"
    fGetObject bucket "lsb-release" outFile defaultGetObjectOptions

    let unmodifiedTime = UTCTime (fromGregorian 2010 11 26) 69857
    step "fGetObject an object which is modified now but requesting as un-modified in past, check for exception"
    resE <- try $ fGetObject bucket "lsb-release" outFile defaultGetObjectOptions {
      gooIfUnmodifiedSince = (Just unmodifiedTime)
      }
    case resE of
      Left exn -> liftIO $ exn @?= ServiceErr "PreconditionFailed" "At least one of the pre-conditions you specified did not hold"
      _        -> return ()

    step "fGetObject an object with no matching etag, check for exception"
    resE1 <- try $ fGetObject bucket "lsb-release" outFile defaultGetObjectOptions {
      gooIfMatch = (Just "invalid-etag")
      }
    case resE1 of
      Left exn -> liftIO $ exn @?= ServiceErr "PreconditionFailed" "At least one of the pre-conditions you specified did not hold"
      _        -> return ()

    step "fGetObject an object with no valid range, check for exception"
    resE2 <- try $ fGetObject bucket "lsb-release" outFile defaultGetObjectOptions {
      gooRange = (Just $ HT.ByteRangeFromTo 100 200)
      }
    case resE2 of
        Left exn -> liftIO $ exn @?= ServiceErr "InvalidRange" "The requested range is not satisfiable"
        _        -> return ()

    step "fGetObject on object with a valid range"
    fGetObject bucket "lsb-release" outFile defaultGetObjectOptions {
      gooRange = (Just $ HT.ByteRangeFrom 1)
      }

    step "fGetObject a non-existent object and check for NoSuchKey exception"
    resE3 <- try $ fGetObject bucket "noSuchKey" outFile defaultGetObjectOptions
    case resE3 of
      Left exn -> liftIO $ exn @?= NoSuchKey
      _        -> return ()

    step "create new multipart upload works"
    uid <- newMultipartUpload bucket "newmpupload" []
    liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

    step "abort a new multipart upload works"
    abortMultipartUpload bucket "newmpupload" uid

    step "delete object works"
    deleteObject bucket "lsb-release"

    step "statObject test"
    let object = "sample"
    step "create an object"
    inputFile <- mkRandFile 0
    fPutObject bucket object inputFile defaultPutObjectOptions

    step "get metadata of the object"
    res <- statObject bucket object defaultGetObjectOptions
    liftIO $ (oiSize res) @?= 0

    step "delete object"
    deleteObject bucket object

lowLevelMultipartTest :: TestTree
lowLevelMultipartTest = funTestWithBucket "Low-level Multipart Test" $
    \step bucket -> do
      -- low-level multipart operation tests.
      let object = "newmpupload"
          mb15 = 15 * 1024 * 1024

      step "Prepare for low-level multipart tests."
      step "create new multipart upload"
      uid <- newMultipartUpload bucket object []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      randFile <- mkRandFile mb15

      step "put object parts 1 of 1"
      h <- liftIO $ SIO.openBinaryFile randFile SIO.ReadMode
      partInfo <- putObjectPart bucket object uid 1 [] $ PayloadH h 0 mb15

      step "complete multipart"
      void $ completeMultipartUpload bucket object uid [partInfo]

      destFile <- mkRandFile 0
      step  "Retrieve the created object and check size"
      fGetObject bucket object destFile defaultGetObjectOptions
      gotSize <- withNewHandle destFile getFileSize
      liftIO $ gotSize == Right (Just mb15) @?
        "Wrong file size of put file after getting"

      step  "Cleanup actions"
      removeObject bucket object

putObjectSizeTest :: TestTree
putObjectSizeTest = funTestWithBucket "PutObject of conduit source with size" $
  \step bucket -> do
      -- putObject test (conduit source, size specified)
      let obj = "msingle"
          mb1 = 1 * 1024 * 1024

      step "Prepare for putObject with from source with size."
      rFile <- mkRandFile mb1

      step "Upload single file."
      putObject bucket obj (CB.sourceFile rFile) (Just mb1) defaultPutObjectOptions

      step "Retrieve and verify file size"
      destFile <- mkRandFile 0
      fGetObject bucket obj destFile defaultGetObjectOptions
      gotSize <- withNewHandle destFile getFileSize
      liftIO $ gotSize == Right (Just mb1) @?
        "Wrong file size of put file after getting"

      step "Cleanup actions"
      deleteObject bucket obj

putObjectNoSizeTest :: TestTree
putObjectNoSizeTest = funTestWithBucket "PutObject of conduit source with no size" $
  \step bucket -> do
      -- putObject test (conduit source, no size specified)
      let obj = "mpart"
          mb70 = 70 * 1024 * 1024

      step "Prepare for putObject with from source without providing size."
      rFile <- mkRandFile mb70

      step "Upload multipart file."
      putObject bucket obj (CB.sourceFile rFile) Nothing defaultPutObjectOptions

      step "Retrieve and verify file size"
      destFile <- mkRandFile 0
      fGetObject bucket obj destFile defaultGetObjectOptions
      gotSize <- withNewHandle destFile getFileSize
      liftIO $ gotSize == Right (Just mb70) @?
        "Wrong file size of put file after getting"

      step "Cleanup actions"
      deleteObject bucket obj

highLevelListingTest :: TestTree
highLevelListingTest = funTestWithBucket "High-level listObjects Test" $
  \step bucket -> do
      step "High-level listObjects Test"
      step "put 3 objects"
      let expectedObjects = ["dir/o1", "dir/dir1/o2", "dir/dir2/o3", "o4"]
          extractObjectsFromList os =
              mapM (\t -> case t of
                       ListItemObject o -> Just $ oiObject o
                       _                -> Nothing) os
          expectedNonRecList = ["o4", "dir/"]
          extractObjectsAndDirsFromList os =
              map (\t -> case t of
                      ListItemObject o -> oiObject o
                      ListItemPrefix d -> d) os

      forM_ expectedObjects $
        \obj -> fPutObject bucket obj "/etc/lsb-release" defaultPutObjectOptions

      step "High-level listing of objects"
      items <- C.runConduit $ listObjects bucket Nothing False C..| sinkList
      liftIO $ assertEqual "Objects/Dirs match failed!" expectedNonRecList $
        extractObjectsAndDirsFromList items

      step "High-level recursive listing of objects"
      objects <- C.runConduit $ listObjects bucket Nothing True C..| sinkList

      liftIO $ assertEqual "Objects match failed!"
        (Just $ sort expectedObjects) $
        extractObjectsFromList objects

      step "High-level listing of objects (version 1)"
      itemsV1 <- C.runConduit $ listObjectsV1 bucket Nothing False C..| sinkList
      liftIO $ assertEqual "Objects/Dirs match failed!" expectedNonRecList $
        extractObjectsAndDirsFromList itemsV1

      step "High-level recursive listing of objects (version 1)"
      objectsV1 <- C.runConduit $ listObjectsV1 bucket Nothing True C..|
                   sinkList

      liftIO $ assertEqual "Objects match failed!"
        (Just $ sort expectedObjects) $
        extractObjectsFromList objectsV1

      let expectedPrefListing = ["dir/o1", "dir/dir1/", "dir/dir2/"]
          expectedPrefListingRec = Just ["dir/dir1/o2", "dir/dir2/o3", "dir/o1"]
      step "High-level listing with prefix"
      prefItems <- C.runConduit $ listObjects bucket (Just "dir/") False C..| sinkList
      liftIO $ assertEqual "Objects/Dirs under prefix match failed!"
        expectedPrefListing $ extractObjectsAndDirsFromList prefItems

      step "High-level listing with prefix recursive"
      prefItemsRec <- C.runConduit $ listObjects bucket (Just "dir/") True C..| sinkList
      liftIO $ assertEqual "Objects/Dirs under prefix match recursive failed!"
        expectedPrefListingRec $ extractObjectsFromList prefItemsRec

      step "High-level listing with prefix (version 1)"
      prefItemsV1 <- C.runConduit $ listObjectsV1 bucket (Just "dir/") False C..| sinkList
      liftIO $ assertEqual "Objects/Dirs under prefix match failed!"
        expectedPrefListing $ extractObjectsAndDirsFromList prefItemsV1

      step "High-level listing with prefix recursive (version 1)"
      prefItemsRecV1 <- C.runConduit $ listObjectsV1 bucket (Just "dir/") True C..| sinkList
      liftIO $ assertEqual "Objects/Dirs under prefix match recursive failed!"
        expectedPrefListingRec $ extractObjectsFromList prefItemsRecV1

      step "Cleanup actions"
      forM_ expectedObjects $
        \obj -> removeObject bucket obj

      step "High-level listIncompleteUploads Test"
      let object = "newmpupload"
      step "create 10 multipart uploads"
      forM_ [1..10::Int] $ \_ -> do
        uid <- newMultipartUpload bucket object []
        liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "High-level listing of incomplete multipart uploads"
      uploads <- C.runConduit $
                 listIncompleteUploads bucket (Just "newmpupload") True C..|
                 sinkList
      liftIO $ length uploads @?= 10

      step "cleanup"
      forM_ uploads $ \(UploadInfo _ uid _ _) ->
                        abortMultipartUpload bucket object uid

      step "High-level listIncompleteParts Test"
      let mb5 = 5 * 1024 * 1024

      step "create a multipart upload"
      uid <- newMultipartUpload bucket object []
      liftIO $ (T.length uid > 0) @? "Got an empty multipartUpload Id."

      step "put object parts 1..10"
      inputFile <- mkRandFile mb5
      h <- liftIO $ SIO.openBinaryFile inputFile SIO.ReadMode
      forM_ [1..10] $ \pnum ->
        putObjectPart bucket object uid pnum [] $ PayloadH h 0 mb5

      step "fetch list parts"
      incompleteParts <- C.runConduit $ listIncompleteParts bucket object uid
                         C..| sinkList
      liftIO $ length incompleteParts @?= 10

      step "cleanup"
      abortMultipartUpload bucket object uid

listingTest :: TestTree
listingTest = funTestWithBucket "Listing Test" $ \step bucket -> do
      step "listObjects' test"
      step "put 10 objects"
      let objects = (\s ->T.concat ["lsb-release", T.pack (show s)]) <$> [1..10::Int]

      forM_ [1..10::Int] $ \s ->
        fPutObject bucket (T.concat ["lsb-release", T.pack (show s)]) "/etc/lsb-release" defaultPutObjectOptions

      step "Simple list"
      res <- listObjects' bucket Nothing Nothing Nothing Nothing
      let expectedObjects = sort objects
      liftIO $ assertEqual "Objects match failed!" expectedObjects
        (map oiObject $ lorObjects res)

      step "Simple list version 1"
      resV1 <- listObjectsV1' bucket Nothing Nothing Nothing Nothing
      let expected = sort $ map (T.concat .
                          ("lsb-release":) .
                          (\x -> [x]) .
                          T.pack .
                          show) [1..10::Int]
      liftIO $ assertEqual "Objects match failed!" expected
        (map oiObject $ lorObjects' resV1)

      step "Cleanup actions"
      forM_ objects $ \obj -> deleteObject bucket obj

      step "listIncompleteUploads' test"
      step "create 10 multipart uploads"
      let object = "newmpupload"
      forM_ [1..10::Int] $ \_ -> do
        uid <- newMultipartUpload bucket object []
        liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "list incomplete multipart uploads"
      incompleteUploads <- listIncompleteUploads' bucket (Just "newmpupload") Nothing
                           Nothing Nothing Nothing
      liftIO $ (length $ lurUploads incompleteUploads) @?= 10

      step "cleanup"
      forM_ (lurUploads incompleteUploads) $
        \(_, uid, _) -> abortMultipartUpload bucket object uid

      step "Basic listIncompleteParts Test"
      let mb5 = 5 * 1024 * 1024

      step "create a multipart upload"
      uid <- newMultipartUpload bucket object []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "put object parts 1..10"
      inputFile <- mkRandFile mb5
      h <- liftIO $ SIO.openBinaryFile inputFile SIO.ReadMode
      forM_ [1..10] $ \pnum ->
        putObjectPart bucket object uid pnum [] $ PayloadH h 0 mb5

      step "fetch list parts"
      listPartsResult <- listIncompleteParts' bucket object uid Nothing Nothing
      liftIO $ (length $ lprParts listPartsResult) @?= 10
      abortMultipartUpload bucket object uid

presignedUrlFunTest :: TestTree
presignedUrlFunTest = funTestWithBucket "presigned Url tests" $
  \step bucket -> do
    let obj = "mydir/myput"
        obj2 = "mydir1/myfile1"

    -- manager for http requests
    mgr <- liftIO $ NC.newManager NC.tlsManagerSettings

    step "PUT object presigned URL - makePresignedUrl"
    putUrl <- makePresignedUrl 3600 HT.methodPut (Just bucket)
           (Just obj) (Just "us-east-1") [] []

    let size1 = 1000 :: Int64
    inputFile <- mkRandFile size1

    -- attempt to upload using the presigned URL
    putResp <- putR size1 inputFile mgr putUrl
    liftIO $ (NC.responseStatus putResp == HT.status200) @?
      "presigned PUT failed"

    step "GET object presigned URL - makePresignedUrl"
    getUrl <- makePresignedUrl 3600 HT.methodGet (Just bucket)
           (Just obj) (Just "us-east-1") [] []

    getResp <- getR mgr getUrl
    liftIO $ (NC.responseStatus getResp == HT.status200) @?
      "presigned GET failed"

    -- read content from file to compare with response above
    bs <- C.runConduit $ CB.sourceFile inputFile C..| CB.sinkLbs
    liftIO $ (bs == NC.responseBody getResp) @?
      "presigned put and get got mismatched data"

    step "PUT object presigned - presignedPutObjectURL"
    putUrl2 <- presignedPutObjectUrl bucket obj2 604800 []

    let size2 = 1200
    testFile <- mkRandFile size2

    putResp2 <- putR size2 testFile mgr putUrl2
    liftIO $ (NC.responseStatus putResp2 == HT.status200) @?
      "presigned PUT failed (presignedPutObjectUrl)"

    step "HEAD object presigned URL - presignedHeadObjectUrl"
    headUrl <- presignedHeadObjectUrl bucket obj2 3600 []

    headResp <- do let req = NC.parseRequest_ $ toS headUrl
                   NC.httpLbs (req {NC.method = HT.methodHead}) mgr
    liftIO $ (NC.responseStatus headResp == HT.status200) @?
      "presigned HEAD failed (presignedHeadObjectUrl)"

    -- check that header info is accurate
    let h = H.fromList $ NC.responseHeaders headResp
        cLen = H.lookupDefault "0" HT.hContentLength h
    liftIO $ (cLen == show size2) @? "Head req returned bad content length"

    step "GET object presigned URL - presignedGetObjectUrl"
    getUrl2 <- presignedGetObjectUrl bucket obj2 3600 [] []

    getResp2 <- getR mgr getUrl2
    liftIO $ (NC.responseStatus getResp2 == HT.status200) @?
      "presigned GET failed (presignedGetObjectUrl)"

    -- read content from file to compare with response above
    bs2 <- C.runConduit $ CB.sourceFile testFile C..| CB.sinkLbs
    liftIO $ (bs2 == NC.responseBody getResp2) @?
      "presigned put and get got mismatched data (presigned*Url)"


    mapM_ (removeObject bucket) [obj, obj2]
  where
    putR size filePath mgr url = do
      let req = NC.parseRequest_ $ toS url
      let req' = req { NC.method = HT.methodPut
                     , NC.requestBody = NC.requestBodySource size $
                                        CB.sourceFile filePath}
      NC.httpLbs req' mgr

    getR mgr url = do
      let req = NC.parseRequest_ $ toS url
      NC.httpLbs req mgr

presignedPostPolicyFunTest :: TestTree
presignedPostPolicyFunTest = funTestWithBucket "Presigned Post Policy tests" $
  \step bucket -> do

    step "presignedPostPolicy basic test"
    now <- liftIO $ Time.getCurrentTime

    let key = "presignedPostPolicyTest/myfile"
        policyConds = [ ppCondBucket bucket
                      , ppCondKey key
                      , ppCondContentLengthRange 1 1000
                      , ppCondContentType "application/octet-stream"
                      , ppCondSuccessActionStatus 200
                      ]

        expirationTime = Time.addUTCTime 3600 now
        postPolicyE = newPostPolicy expirationTime policyConds

        size = 1000 :: Int64

    inputFile <- mkRandFile size

    case postPolicyE of
      Left err -> liftIO $ assertFailure $ show err
      Right postPolicy -> do
        (url, formData) <- presignedPostPolicy postPolicy
        -- liftIO (print url) >> liftIO (print formData)
        result <- liftIO $ postForm url formData inputFile
        liftIO $ (NC.responseStatus result == HT.status200) @?
          "presigned POST failed"

    mapM_ (removeObject bucket) [key]
    where

      postForm url formData inputFile = do
        req <- NC.parseRequest $ toS url
        let parts = map (\(x, y) -> Form.partBS x y) $
                    H.toList formData
            parts' = parts ++ [Form.partFile "file" inputFile]
        req' <- Form.formDataBody parts' req
        mgr <- NC.newManager NC.tlsManagerSettings
        NC.httpLbs req' mgr

bucketPolicyFunTest :: TestTree
bucketPolicyFunTest = funTestWithBucket "Bucket Policy tests" $
  \step bucket -> do

    step "bucketPolicy basic test - no policy exception"
    resE <- try $ getBucketPolicy bucket
    case resE of
      Left exn -> liftIO $ exn @?= ServiceErr "NoSuchBucketPolicy" "The bucket policy does not exist"
      _        -> return ()

    resE' <- try $ setBucketPolicy bucket T.empty
    case resE' of
      Left exn -> liftIO $ exn @?= ServiceErr "NoSuchBucketPolicy" "The bucket policy does not exist"
      _        -> return ()

    let expectedPolicyJSON = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Action\":[\"s3:GetBucketLocation\",\"s3:ListBucket\"],\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Resource\":[\"arn:aws:s3:::testbucket\"]},{\"Action\":[\"s3:GetObject\"],\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Resource\":[\"arn:aws:s3:::testbucket/*\"]}]}"

    step "try a malformed policy, expect error"
    resE'' <- try $ setBucketPolicy bucket expectedPolicyJSON
    case resE'' of
      Left exn -> liftIO $ exn @?= ServiceErr "MalformedPolicy" "Policy has invalid resource."
      _        -> return ()

    let expectedPolicyJSON' = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Action\":[\"s3:GetBucketLocation\",\"s3:ListBucket\"],\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Resource\":[\"arn:aws:s3:::" <> bucket <> "\"]},{\"Action\":[\"s3:GetObject\"],\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Resource\":[\"arn:aws:s3:::" <> bucket <> "/*\"]}]}"

    step "set bucket policy"
    setBucketPolicy bucket expectedPolicyJSON'

    let obj = "myobject"

    step "verify bucket policy: (1) create `myobject`"
    putObject bucket obj (replicateC 100 "c") Nothing defaultPutObjectOptions

    step "verify bucket policy: (2) get `myobject` anonymously"
    connInfo <- asks mcConnInfo
    let proto = bool "http://" "https://" $ connectIsSecure connInfo
        url = BS.concat [proto, getHostAddr connInfo, "/", toS bucket,
                         "/", toS obj]
    respE <- liftIO $ (fmap (Right . toS) $ NC.simpleHttp $ toS url) `catch`
               (\(e :: NC.HttpException) -> return $ Left (show e :: Text))
    case respE of
        Left err -> liftIO $ assertFailure $ show err
        Right s  -> liftIO $ s @?= (BS.concat $ replicate 100 "c")

    deleteObject bucket obj

    step "delete bucket policy"
    setBucketPolicy bucket T.empty

multipartTest :: TestTree
multipartTest = funTestWithBucket "Multipart Tests" $
  \step bucket -> do
    step "Prepare for putObjectInternal with non-seekable file, with size."
    step "Upload multipart file."
    let mb80 = 80 * 1024 * 1024
        obj = "mpart"

    void $ putObjectInternal bucket obj defaultPutObjectOptions $ ODFile "/dev/zero" (Just mb80)

    step "Retrieve and verify file size"
    destFile <- mkRandFile 0
    fGetObject bucket obj destFile defaultGetObjectOptions
    gotSize <- withNewHandle destFile getFileSize
    liftIO $ gotSize == Right (Just mb80) @?
      "Wrong file size of put file after getting"

    step "Cleanup actions"
    removeObject bucket obj

    step "cleanup"
    removeObject bucket "big"

    step "Prepare for removeIncompleteUpload"
    -- low-level multipart operation tests.
    let object = "newmpupload"
        kb5 = 5 * 1024

    step "create new multipart upload"
    uid <- newMultipartUpload bucket object []
    liftIO $ (T.length uid > 0) @? "Got an empty multipartUpload Id."

    randFile <- mkRandFile kb5

    step "upload 2 parts"
    forM_ [1,2] $ \partNum -> do
      h <- liftIO $ SIO.openBinaryFile randFile SIO.ReadMode
      void $ putObjectPart bucket object uid partNum [] $ PayloadH h 0 kb5

    step "remove ongoing upload"
    removeIncompleteUpload bucket object
    uploads <- C.runConduit $ listIncompleteUploads bucket (Just object) False
               C..| sinkList
    liftIO $ (null uploads) @? "removeIncompleteUploads didn't complete successfully"

putObjectContentTypeTest :: TestTree
putObjectContentTypeTest = funTestWithBucket "putObject contentType tests" $
  \step bucket -> do
    step "fPutObject content type test"
    let object = "xxx-content-type"
        size1 = 100 :: Int64

    step "create server object with content-type"
    inputFile <- mkRandFile size1
    fPutObject bucket object inputFile defaultPutObjectOptions {
      pooContentType = Just "application/javascript"
      }

    -- retrieve obj info to check
    oi <- headObject bucket object []
    let m = oiMetadata oi

    step "Validate content-type"
    liftIO $ assertEqual "Content-Type did not match" (Just "application/javascript") (H.lookup "Content-Type" m)

    step "upload object with content-encoding set to identity"
    fPutObject bucket object inputFile defaultPutObjectOptions {
      pooContentEncoding = Just "identity"
      }

    oiCE <- headObject bucket object []
    let m' = oiMetadata oiCE

    step "Validate content-encoding"
    liftIO $ assertEqual "Content-Encoding did not match" (Just "identity")
      (H.lookup "Content-Encoding" m')

    step "Cleanup actions"

    removeObject bucket object

putObjectContentLanguageTest :: TestTree
putObjectContentLanguageTest = funTestWithBucket "putObject contentLanguage tests" $
  \step bucket -> do
    step "fPutObject content language test"
    let object = "xxx-content-language"
        size1 = 100 :: Int64

    step "create server object with content-language"
    inputFile <- mkRandFile size1
    fPutObject bucket object inputFile defaultPutObjectOptions {
      pooContentLanguage = Just "en-US"
      }

    -- retrieve obj info to check
    oi <- headObject bucket object []
    let m = oiMetadata oi

    step "Validate content-language"
    liftIO $ assertEqual "content-language did not match" (Just "en-US")
      (H.lookup "Content-Language" m)
    step "Cleanup actions"

    removeObject bucket object

putObjectUserMetadataTest :: TestTree
putObjectUserMetadataTest = funTestWithBucket "putObject user-metadata test" $
  \step bucket -> do
    step "putObject user-metadata test"
    let object = "object-with-metadata"
        size1 = 100 :: Int64

    step "create server object with usermetdata"
    inputFile <- mkRandFile size1

    fPutObject bucket object inputFile defaultPutObjectOptions {
      pooUserMetadata = [ ("x-Amz-meta-mykey1", "myval1")
                        , ("mykey2", "myval2")
                        ]
      }

    step "Validate user-metadata"
    -- retrieve obj info to check
    oi <- headObject bucket object []
    let m = oiUserMetadata oi
        -- need to do a case-insensitive comparison
        sortedMeta = sort $ map (\(k, v) -> (T.toLower k, T.toLower v)) $
                     H.toList m
        ref = sort [("mykey1", "myval1"), ("mykey2", "myval2")]

    liftIO $ (sortedMeta == ref) @? "Metadata mismatch!"

    step "Cleanup actions"
    removeObject bucket object

getObjectTest :: TestTree
getObjectTest = funTestWithBucket "getObject test" $
  \step bucket -> do
    step "putObject with some metadata"
    let object = "object-with-metadata"
        size1 = 100 :: Int64

    inputFile <- mkRandFile size1
    fPutObject bucket object inputFile defaultPutObjectOptions {
      pooUserMetadata = [ ("x-Amz-meta-mykey1", "myval1")
                        , ("mykey2", "myval2")
                        ]
      }

    step "get the object - check the metadata matches"
    -- retrieve obj info to check
    gor <- getObject bucket object defaultGetObjectOptions
    let m = oiUserMetadata $ gorObjectInfo gor
        -- need to do a case-insensitive comparison
        sortedMeta = sort $ map (\(k, v) -> (T.toLower k, T.toLower v)) $
                     H.toList m
        ref = sort [("mykey1", "myval1"), ("mykey2", "myval2")]

    liftIO $ (sortedMeta == ref) @? "Metadata mismatch!"

    step "get the object content"
    getObjectHash <- hashSHA256FromSource $ gorObjectStream gor
    inputHash <- hashSHA256FromSource $ C.sourceFile inputFile
    liftIO $ (getObjectHash == inputHash) @? "Input file and output file mismatched!"

    step "Cleanup actions"
    removeObject bucket object

putObjectStorageClassTest :: TestTree
putObjectStorageClassTest = funTestWithBucket "putObject storageClass tests" $
  \step bucket -> do
    step "fPutObject storage class test"
    let object = "xxx-storage-class-standard"
        object' = "xxx-storage-class-reduced"
        object'' = "xxx-storage-class-invalid"
        size1 = 100 :: Int64
        size0 = 0 :: Int64

    step "create server objects with storageClass"
    inputFile <- mkRandFile size1
    inputFile' <- mkRandFile size1
    inputFile'' <- mkRandFile size0

    fPutObject bucket object inputFile defaultPutObjectOptions {
      pooStorageClass = Just "STANDARD"
      }

    fPutObject bucket object' inputFile' defaultPutObjectOptions {
      pooStorageClass = Just "REDUCED_REDUNDANCY"
      }

    removeObject bucket object

    -- retrieve obj info to check
    oi' <- headObject bucket object' []
    let m' = oiMetadata oi'

    step "Validate x-amz-storage-class rrs"
    liftIO $ assertEqual "storageClass did not match" (Just "REDUCED_REDUNDANCY")
      (H.lookup "X-Amz-Storage-Class" m')

    fpE <- try $ fPutObject bucket object'' inputFile'' defaultPutObjectOptions {
      pooStorageClass = Just "INVALID_STORAGE_CLASS"
      }
    case fpE of
      Left exn -> liftIO $ exn @?= ServiceErr "InvalidStorageClass" "Invalid storage class."
      _        -> return ()

    step "Cleanup actions"

    removeObject bucket object'

copyObjectTests :: TestTree
copyObjectTests = funTestWithBucket "copyObject related tests" $
  \step bucket -> do
    step "copyObjectSingle basic tests"
    let object = "xxx"
        objCopy = "xxxCopy"
        size1 = 100 :: Int64

    step "create server object to copy"
    inputFile <- mkRandFile size1
    fPutObject bucket object inputFile defaultPutObjectOptions

    step "copy object"
    let srcInfo = defaultSourceInfo { srcBucket = bucket, srcObject = object}
    (etag, modTime) <- copyObjectSingle bucket objCopy srcInfo []

    -- retrieve obj info to check
    oi <- headObject bucket objCopy []
    let t = oiModTime oi
    let e = oiETag oi
    let s = oiSize oi

    let isMTimeDiffOk = abs (diffUTCTime modTime t) < 1.0

    liftIO $ (s == size1 && e == etag && isMTimeDiffOk) @?
      "Copied object did not match expected."

    step "cleanup actions"
    removeObject bucket object
    removeObject bucket objCopy

    step "copyObjectPart basic tests"
    let srcObj = "XXX"
        copyObj = "XXXCopy"

    step "Prepare"
    let mb15 = 15 * 1024 * 1024
        mb5 = 5 * 1024 * 1024
    randFile <- mkRandFile mb15
    fPutObject bucket srcObj randFile defaultPutObjectOptions

    step "create new multipart upload"
    uid <- newMultipartUpload bucket copyObj []
    liftIO $ (T.length uid > 0) @? "Got an empty multipartUpload Id."

    step "put object parts 1-3"
    let srcInfo' = defaultSourceInfo { srcBucket = bucket, srcObject = srcObj }
        dstInfo' = defaultDestinationInfo { dstBucket = bucket, dstObject = copyObj }
    parts <- forM [1..3] $ \p -> do
      (etag', _) <- copyObjectPart dstInfo' srcInfo'{
        srcRange = Just $ (,) ((p-1)*mb5) ((p-1)*mb5 + (mb5 - 1))
        } uid (fromIntegral p) []
      return (fromIntegral p, etag')

    step "complete multipart"
    void $ completeMultipartUpload bucket copyObj uid parts

    step "verify copied object size"
    oi' <- headObject bucket copyObj []
    let s' = oiSize oi'

    liftIO $ (s' == mb15) @? "Size failed to match"

    step "Cleanup actions"
    removeObject bucket srcObj
    removeObject bucket copyObj

    step "copyObject basic tests"
    let srcs = ["XXX", "XXXL"]
        copyObjs = ["XXXCopy", "XXXLCopy"]
        sizes = map (* (1024 * 1024)) [15, 65]

    step "Prepare"
    forM_ (zip srcs sizes) $ \(src, size) -> do
      inputFile' <- mkRandFile size
      fPutObject bucket src inputFile' defaultPutObjectOptions

    step "make small and large object copy"
    forM_ (zip copyObjs srcs) $ \(cp, src) ->
      copyObject defaultDestinationInfo {dstBucket = bucket, dstObject = cp} defaultSourceInfo {srcBucket = bucket, srcObject = src}

    step "verify uploaded objects"
    uploadedSizes <- fmap oiSize <$> forM copyObjs (\o -> headObject bucket o [])

    liftIO $ (sizes == uploadedSizes) @? "Uploaded obj sizes failed to match"

    forM_ (srcs ++ copyObjs) (removeObject bucket)

    step "copyObject with offset test "
    let src = "XXX"
        size = 15 * 1024 * 1024

    step "Prepare"
    inputFile' <- mkRandFile size
    fPutObject bucket src inputFile' defaultPutObjectOptions

    step "copy last 10MiB of object"
    copyObject defaultDestinationInfo { dstBucket = bucket, dstObject = copyObj } defaultSourceInfo {
        srcBucket = bucket
      , srcObject = src
      , srcRange = Just $ (,) (5 * 1024 * 1024) (size - 1)
      }

    step "verify uploaded object"
    cSize <- oiSize <$> headObject bucket copyObj []

    liftIO $ (cSize == 10 * 1024 * 1024) @? "Uploaded obj size mismatched!"

    forM_ [src, copyObj] (removeObject bucket)

getNPutSSECTest :: TestTree
getNPutSSECTest =
    funTestWithBucket "Get and Put SSE-C Test" $ \step bucket -> do
       -- Skip this test if the server is not using TLS as encryption is
       -- disabled anyway.
       isTLSConn <- asks (connectIsSecure . mcConnInfo)
       if isTLSConn
           then do step "Make an encryption key"
                   key <- case mkSSECKey $ BS.pack [0..31] of
                            Nothing -> liftIO $ assertFailure "This should not happen"
                            Just k  -> return k

                   let mb1 = 1024*1024
                       obj = "1"
                   step "Upload an object using the encryption key"
                   rFile <- mkRandFile mb1
                   let putOpts = defaultPutObjectOptions { pooSSE = Just $ SSEC key }
                   fPutObject bucket obj rFile putOpts

                   step "Stat object without key - should fail"
                   headRes <- try $ statObject bucket obj defaultGetObjectOptions
                   case headRes of
                     Right _ -> liftIO $ assertFailure "Cannot perform head object on encrypted object without specifying key"
                     Left ex@(NC.HttpExceptionRequest _ (NC.StatusCodeException rsp _))
                         | NC.responseStatus rsp == HT.status400 -> return ()
                         | otherwise -> liftIO $ assertFailure $ "Unexpected err: " ++ show ex
                     Left ex -> liftIO $ assertFailure $ "Unexpected err: " ++ show ex

                   step "Get file and check length"
                   dstFile <- mkRandFile 0
                   let getOpts = defaultGetObjectOptions { gooSSECKey = Just key }
                   fGetObject bucket obj dstFile getOpts

                   gotSize <- withNewHandle dstFile getFileSize
                   liftIO $ gotSize == Right (Just mb1) @?
                       "Wrong file size of object when getting"

                   step "Cleanup"
                   deleteObject bucket obj

           else step "Skipping encryption test as server is not using TLS"
