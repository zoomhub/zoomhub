--
-- MinIO Haskell SDK, (C) 2017 MinIO, Inc.
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

{-# LANGUAGE QuasiQuotes #-}
module Network.Minio.XmlParser.Test
  ( xmlParserTests
  ) where

import qualified Data.HashMap.Strict       as H
import           Data.Time                 (fromGregorian)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.RawString.QQ         (r)
import           UnliftIO                  (MonadUnliftIO)

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.TestHelpers
import           Network.Minio.XmlParser

xmlParserTests :: TestTree
xmlParserTests = testGroup "XML Parser Tests"
  [ testCase "Test parseLocation" testParseLocation
  , testCase "Test parseNewMultipartUpload" testParseNewMultipartUpload
  , testCase "Test parseListObjectsResponse" testParseListObjectsResult
  , testCase "Test parseListObjectsV1Response" testParseListObjectsV1Result
  , testCase "Test parseListUploadsresponse" testParseListIncompleteUploads
  , testCase "Test parseCompleteMultipartUploadResponse" testParseCompleteMultipartUploadResponse
  , testCase "Test parseListPartsResponse" testParseListPartsResponse
  , testCase "Test parseCopyObjectResponse" testParseCopyObjectResponse
  , testCase "Test parseNotification" testParseNotification
  , testCase "Test parseSelectProgress" testParseSelectProgress
  ]

tryValidationErr :: (MonadUnliftIO m) => m a -> m (Either MErrV a)
tryValidationErr act = try act

assertValidtionErr :: MErrV -> Assertion
assertValidtionErr e = assertFailure $ "Failed due to validation error => " ++ show e

eitherValidationErr :: Either MErrV a -> (a -> Assertion) -> Assertion
eitherValidationErr (Left e) _  = assertValidtionErr e
eitherValidationErr (Right a) f = f a

testParseLocation :: Assertion
testParseLocation = do
  -- 1. Test parsing of an invalid location constraint xml.
  parseResE <- tryValidationErr $ parseLocation "ClearlyInvalidXml"
  when (isRight parseResE) $
    assertFailure $ "Parsing should have failed => " ++ show parseResE

  forM_ cases $ \(xmldata, expectedLocation) -> do
    parseLocE <- tryValidationErr $ parseLocation xmldata
    either assertValidtionErr (@?= expectedLocation) parseLocE
  where
    cases =  [
      -- 2. Test parsing of a valid location xml.
      ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
       \<LocationConstraint xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">EU</LocationConstraint>",
       "EU"
      )
      ,
      -- 3. Test parsing of a valid, empty location xml.
      ("<LocationConstraint xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"/>",
       "us-east-1"
      )
      ]


testParseNewMultipartUpload :: Assertion
testParseNewMultipartUpload = do
  forM_ cases $ \(xmldata, expectedUploadId) -> do
    parsedUploadIdE <- tryValidationErr $ runTestNS $ parseNewMultipartUpload xmldata
    eitherValidationErr parsedUploadIdE (@?= expectedUploadId)
  where
    cases = [
      ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
       \<InitiateMultipartUploadResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
       \  <Bucket>example-bucket</Bucket>\
       \  <Key>example-object</Key>\
       \  <UploadId>VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA</UploadId>\
       \</InitiateMultipartUploadResult>",
       "VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA"
      ),
      ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
       \<InitiateMultipartUploadResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
       \  <Bucket>example-bucket</Bucket>\
       \  <Key>example-object</Key>\
       \  <UploadId>EXAMPLEJZ6e0YupT2h66iePQCc9IEbYbDUy4RTpMeoSMLPRp8Z5o1u8feSRonpvnWsKKG35tI2LB9VDPiCgTy.Gq2VxQLYjrue4Nq.NBdqI-</UploadId>\
       \</InitiateMultipartUploadResult>",
       "EXAMPLEJZ6e0YupT2h66iePQCc9IEbYbDUy4RTpMeoSMLPRp8Z5o1u8feSRonpvnWsKKG35tI2LB9VDPiCgTy.Gq2VxQLYjrue4Nq.NBdqI-"
      )
      ]

testParseListObjectsResult :: Assertion
testParseListObjectsResult = do
  let
    xmldata = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
              \<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
              \<Name>bucket</Name>\
              \<Prefix/>\
              \<NextContinuationToken>opaque</NextContinuationToken>\
              \<KeyCount>1000</KeyCount>\
              \<MaxKeys>1000</MaxKeys>\
              \<IsTruncated>true</IsTruncated>\
              \<Contents>\
              \<Key>my-image.jpg</Key>\
              \<LastModified>2009-10-12T17:50:30.000Z</LastModified>\
              \<ETag>&quot;fba9dede5f27731c9771645a39863328&quot;</ETag>\
              \<Size>434234</Size>\
              \<StorageClass>STANDARD</StorageClass>\
              \</Contents>\
              \</ListBucketResult>"

    expectedListResult = ListObjectsResult True (Just "opaque") [object1] []
    object1 = ObjectInfo "my-image.jpg" modifiedTime1 "\"fba9dede5f27731c9771645a39863328\"" 434234 H.empty H.empty
    modifiedTime1 = flip UTCTime 64230 $ fromGregorian 2009 10 12

  parsedListObjectsResult <- tryValidationErr $ runTestNS $ parseListObjectsResponse xmldata
  eitherValidationErr parsedListObjectsResult (@?= expectedListResult)

testParseListObjectsV1Result :: Assertion
testParseListObjectsV1Result = do
  let
    xmldata = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
              \<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
              \<Name>bucket</Name>\
              \<Prefix/>\
              \<NextMarker>my-image1.jpg</NextMarker>\
              \<KeyCount>1000</KeyCount>\
              \<MaxKeys>1000</MaxKeys>\
              \<IsTruncated>true</IsTruncated>\
              \<Contents>\
              \<Key>my-image.jpg</Key>\
              \<LastModified>2009-10-12T17:50:30.000Z</LastModified>\
              \<ETag>&quot;fba9dede5f27731c9771645a39863328&quot;</ETag>\
              \<Size>434234</Size>\
              \<StorageClass>STANDARD</StorageClass>\
              \</Contents>\
              \</ListBucketResult>"

    expectedListResult = ListObjectsV1Result True (Just "my-image1.jpg") [object1] []
    object1 = ObjectInfo "my-image.jpg" modifiedTime1 "\"fba9dede5f27731c9771645a39863328\"" 434234 H.empty H.empty
    modifiedTime1 = flip UTCTime 64230 $ fromGregorian 2009 10 12

  parsedListObjectsV1Result <- tryValidationErr $ runTestNS $ parseListObjectsV1Response xmldata
  eitherValidationErr parsedListObjectsV1Result (@?= expectedListResult)

testParseListIncompleteUploads :: Assertion
testParseListIncompleteUploads = do
  let
    xmldata = "<ListMultipartUploadsResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
  \<Bucket>example-bucket</Bucket>\
  \<KeyMarker/>\
  \<UploadIdMarker/>\
  \<NextKeyMarker>sample.jpg</NextKeyMarker>\
  \<NextUploadIdMarker>Xgw4MJT6ZPAVxpY0SAuGN7q4uWJJM22ZYg1W99trdp4tpO88.PT6.MhO0w2E17eutfAvQfQWoajgE_W2gpcxQw--</NextUploadIdMarker>\
  \<Delimiter>/</Delimiter>\
  \<Prefix/>\
  \<MaxUploads>1000</MaxUploads>\
  \<IsTruncated>false</IsTruncated>\
  \<Upload>\
    \<Key>sample.jpg</Key>\
    \<UploadId>Agw4MJT6ZPAVxpY0SAuGN7q4uWJJM22ZYg1N99trdp4tpO88.PT6.MhO0w2E17eutfAvQfQWoajgE_W2gpcxQw--</UploadId>\
    \<Initiator>\
      \<ID>314133b66967d86f031c7249d1d9a80249109428335cd0ef1cdc487b4566cb1b</ID>\
      \<DisplayName>s3-nickname</DisplayName>\
    \</Initiator>\
    \<Owner>\
      \<ID>314133b66967d86f031c7249d1d9a80249109428335cd0ef1cdc487b4566cb1b</ID>\
      \<DisplayName>s3-nickname</DisplayName>\
    \</Owner>\
    \<StorageClass>STANDARD</StorageClass>\
    \<Initiated>2010-11-26T19:24:17.000Z</Initiated>\
  \</Upload>\
  \<CommonPrefixes>\
    \<Prefix>photos/</Prefix>\
  \</CommonPrefixes>\
  \<CommonPrefixes>\
    \<Prefix>videos/</Prefix>\
  \</CommonPrefixes>\
  \</ListMultipartUploadsResult>"
    expectedListResult = ListUploadsResult False (Just "sample.jpg") (Just "Xgw4MJT6ZPAVxpY0SAuGN7q4uWJJM22ZYg1W99trdp4tpO88.PT6.MhO0w2E17eutfAvQfQWoajgE_W2gpcxQw--") uploads prefixes
    uploads = [("sample.jpg", "Agw4MJT6ZPAVxpY0SAuGN7q4uWJJM22ZYg1N99trdp4tpO88.PT6.MhO0w2E17eutfAvQfQWoajgE_W2gpcxQw--", initTime)]
    initTime = UTCTime (fromGregorian 2010 11 26) 69857
    prefixes = ["photos/", "videos/"]

  parsedListUploadsResult <- tryValidationErr $ runTestNS $ parseListUploadsResponse xmldata
  eitherValidationErr parsedListUploadsResult (@?= expectedListResult)


testParseCompleteMultipartUploadResponse :: Assertion
testParseCompleteMultipartUploadResponse = do
  let
    xmldata = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
\<CompleteMultipartUploadResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
  \<Location>http://Example-Bucket.s3.amazonaws.com/Example-Object</Location>\
  \<Bucket>Example-Bucket</Bucket>\
  \<Key>Example-Object</Key>\
  \<ETag>\"3858f62230ac3c915f300c664312c11f-9\"</ETag>\
\</CompleteMultipartUploadResult>"
    expectedETag = "\"3858f62230ac3c915f300c664312c11f-9\""

  parsedETagE <- runExceptT $ runTestNS $ parseCompleteMultipartUploadResponse xmldata
  eitherValidationErr parsedETagE (@?= expectedETag)

testParseListPartsResponse :: Assertion
testParseListPartsResponse = do
  let
    xmldata = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
\<ListPartsResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
  \<Bucket>example-bucket</Bucket>\
  \<Key>example-object</Key>\
  \<UploadId>XXBsb2FkIElEIGZvciBlbHZpbmcncyVcdS1tb3ZpZS5tMnRzEEEwbG9hZA</UploadId>\
  \<Initiator>\
      \<ID>arn:aws:iam::111122223333:user/some-user-11116a31-17b5-4fb7-9df5-b288870f11xx</ID>\
      \<DisplayName>umat-user-11116a31-17b5-4fb7-9df5-b288870f11xx</DisplayName>\
  \</Initiator>\
  \<Owner>\
    \<ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeebf76c078efc7c6caea54ba06a</ID>\
    \<DisplayName>someName</DisplayName>\
  \</Owner>\
  \<StorageClass>STANDARD</StorageClass>\
  \<PartNumberMarker>1</PartNumberMarker>\
  \<NextPartNumberMarker>3</NextPartNumberMarker>\
  \<MaxParts>2</MaxParts>\
  \<IsTruncated>true</IsTruncated>\
  \<Part>\
    \<PartNumber>2</PartNumber>\
    \<LastModified>2010-11-10T20:48:34.000Z</LastModified>\
    \<ETag>\"7778aef83f66abc1fa1e8477f296d394\"</ETag>\
    \<Size>10485760</Size>\
  \</Part>\
  \<Part>\
    \<PartNumber>3</PartNumber>\
    \<LastModified>2010-11-10T20:48:33.000Z</LastModified>\
    \<ETag>\"aaaa18db4cc2f85cedef654fccc4a4x8\"</ETag>\
    \<Size>10485760</Size>\
  \</Part>\
\</ListPartsResult>"

    expectedListResult = ListPartsResult True (Just 3) [part1, part2]
    part1 = ObjectPartInfo 2 "\"7778aef83f66abc1fa1e8477f296d394\"" 10485760 modifiedTime1
    modifiedTime1 = flip UTCTime 74914 $ fromGregorian 2010 11 10
    part2 = ObjectPartInfo 3 "\"aaaa18db4cc2f85cedef654fccc4a4x8\"" 10485760 modifiedTime2
    modifiedTime2 = flip UTCTime 74913 $ fromGregorian 2010 11 10

  parsedListPartsResult <- runExceptT $ runTestNS $ parseListPartsResponse xmldata
  eitherValidationErr parsedListPartsResult (@?= expectedListResult)

testParseCopyObjectResponse :: Assertion
testParseCopyObjectResponse = do
  let
    cases = [ ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
\<CopyObjectResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
   \<LastModified>2009-10-28T22:32:00.000Z</LastModified>\
   \<ETag>\"9b2cf535f27731c974343645a3985328\"</ETag>\
\</CopyObjectResult>",
              ("\"9b2cf535f27731c974343645a3985328\"",
               UTCTime (fromGregorian 2009 10 28) 81120))
            , ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
\<CopyPartResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
   \<LastModified>2009-10-28T22:32:00.000Z</LastModified>\
   \<ETag>\"9b2cf535f27731c974343645a3985328\"</ETag>\
\</CopyPartResult>",
              ("\"9b2cf535f27731c974343645a3985328\"",
              UTCTime (fromGregorian 2009 10 28) 81120))]

  forM_ cases $ \(xmldata, (etag, modTime)) -> do
    parseResult <- runExceptT $ runTestNS $ parseCopyObjectResponse xmldata
    eitherValidationErr parseResult (@?= (etag, modTime))

testParseNotification :: Assertion
testParseNotification = do
  let
    cases = [ ("<NotificationConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
\  <TopicConfiguration>\
\    <Id>YjVkM2Y0YmUtNGI3NC00ZjQyLWEwNGItNDIyYWUxY2I0N2M4</Id>\
\    <Topic>arn:aws:sns:us-east-1:account-id:s3notificationtopic2</Topic>\
\    <Event>s3:ReducedRedundancyLostObject</Event>\
\    <Event>s3:ObjectCreated:*</Event>\
\  </TopicConfiguration>\
\</NotificationConfiguration>",
               Notification []
                [ NotificationConfig
                  "YjVkM2Y0YmUtNGI3NC00ZjQyLWEwNGItNDIyYWUxY2I0N2M4"
                  "arn:aws:sns:us-east-1:account-id:s3notificationtopic2"
                  [ReducedRedundancyLostObject, ObjectCreated] defaultFilter
                ]
                [])
            , ("<NotificationConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
\  <CloudFunctionConfiguration>\
\    <Id>ObjectCreatedEvents</Id>\
\    <CloudFunction>arn:aws:lambda:us-west-2:35667example:function:CreateThumbnail</CloudFunction>\
\    <Event>s3:ObjectCreated:*</Event>\
\  </CloudFunctionConfiguration>\
\  <QueueConfiguration>\
\      <Id>1</Id>\
\      <Filter>\
\          <S3Key>\
\              <FilterRule>\
\                  <Name>prefix</Name>\
\                  <Value>images/</Value>\
\              </FilterRule>\
\              <FilterRule>\
\                  <Name>suffix</Name>\
\                  <Value>.jpg</Value>\
\              </FilterRule>\
\          </S3Key>\
\     </Filter>\
\     <Queue>arn:aws:sqs:us-west-2:444455556666:s3notificationqueue</Queue>\
\     <Event>s3:ObjectCreated:Put</Event>\
\  </QueueConfiguration>\
\  <TopicConfiguration>\
\    <Topic>arn:aws:sns:us-east-1:356671443308:s3notificationtopic2</Topic>\
\    <Event>s3:ReducedRedundancyLostObject</Event>\
\  </TopicConfiguration>\
\  <QueueConfiguration>\
\    <Queue>arn:aws:sqs:us-east-1:356671443308:s3notificationqueue</Queue>\
\    <Event>s3:ObjectCreated:*</Event>\
\  </QueueConfiguration>)\
\</NotificationConfiguration>",
               Notification [ NotificationConfig
                              "1" "arn:aws:sqs:us-west-2:444455556666:s3notificationqueue"
                              [ObjectCreatedPut]
                              (Filter $ FilterKey $ FilterRules
                               [FilterRule "prefix" "images/",
                                FilterRule "suffix" ".jpg"])
                            , NotificationConfig
                              "" "arn:aws:sqs:us-east-1:356671443308:s3notificationqueue"
                              [ObjectCreated] defaultFilter
                            ]
                            [ NotificationConfig
                              "" "arn:aws:sns:us-east-1:356671443308:s3notificationtopic2"
                              [ReducedRedundancyLostObject] defaultFilter
                            ]
                            [ NotificationConfig
                              "ObjectCreatedEvents" "arn:aws:lambda:us-west-2:35667example:function:CreateThumbnail"
                              [ObjectCreated] defaultFilter
                            ])
            ]

  forM_ cases $ \(xmldata, val) -> do
    result <- runExceptT $ runTestNS $ parseNotification xmldata
    eitherValidationErr result (@?= val)

-- | Tests parsing of both progress and stats
testParseSelectProgress :: Assertion
testParseSelectProgress = do
    let cases = [ ([r|<?xml version="1.0" encoding="UTF-8"?>
<Progress>
     <BytesScanned>512</BytesScanned>
     <BytesProcessed>1024</BytesProcessed>
     <BytesReturned>1024</BytesReturned>
</Progress>|] , Progress 512 1024 1024)
                , ([r|<?xml version="1.0" encoding="UTF-8"?>
<Stats>
     <BytesScanned>512</BytesScanned>
     <BytesProcessed>1024</BytesProcessed>
     <BytesReturned>1024</BytesReturned>
</Stats>|], Progress 512 1024 1024)
                ]

    forM_ cases $ \(xmldata, progress) -> do
        result <- runExceptT $ parseSelectProgress xmldata
        eitherValidationErr result (@?= progress)
