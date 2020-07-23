--
-- MinIO Haskell SDK, (C) 2017, 2018 MinIO, Inc.
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

module Network.Minio.API.Test
  ( bucketNameValidityTests
  , objectNameValidityTests
  , parseServerInfoJSONTest
  , parseHealStatusTest
  , parseHealStartRespTest
  ) where

import           Data.Aeson             (eitherDecode)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib.Prelude

import           Network.Minio.AdminAPI
import           Network.Minio.API

assertBool' :: Bool -> Assertion
assertBool' = assertBool "Test failed!"

bucketNameValidityTests :: TestTree
bucketNameValidityTests = testGroup "Bucket Name Validity Tests"
  [ testCase "Too short 1" $ assertBool' $ not $ isValidBucketName ""
  , testCase "Too short 2" $ assertBool' $ not $ isValidBucketName "ab"
  , testCase "Too long 1" $ assertBool' $ not $ isValidBucketName "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  , testCase "Has upper case" $ assertBool' $ not $ isValidBucketName "ABCD"
  , testCase "Has punctuation" $ assertBool' $ not $ isValidBucketName "abc,2"
  , testCase "Has hyphen at end" $ assertBool' $ not $ isValidBucketName "abc-"
  , testCase "Has consecutive dot" $ assertBool' $ not $ isValidBucketName "abck..eedg"
  , testCase "Looks like IP" $  assertBool' $ not $ isValidBucketName "10.0.0.1"
  , testCase "Valid bucket name 1" $ assertBool' $ isValidBucketName "abcd.pqeq.rea"
  , testCase "Valid bucket name 2" $ assertBool' $ isValidBucketName "abcdedgh1d"
  , testCase "Valid bucket name 3" $ assertBool' $ isValidBucketName "abc-de-dg-h1d"
  ]

objectNameValidityTests :: TestTree
objectNameValidityTests = testGroup "Object Name Validity Tests"
  [ testCase "Empty name" $ assertBool' $ not $ isValidObjectName ""
  , testCase "Has unicode characters" $ assertBool' $ isValidObjectName "日本国"
  ]

parseServerInfoJSONTest :: TestTree
parseServerInfoJSONTest = testGroup "Parse MinIO Admin API ServerInfo JSON test" $
  map (\(tName, tDesc, tfn, tVal) -> testCase tName $ assertBool tDesc $
        tfn (eitherDecode tVal :: Either [Char] [ServerInfo])) testCases
  where
    testCases = [ ("FSBackend", "Verify server info json parsing for FS backend", isRight, fsJSON)
                , ("Erasure Backend", "Verify server info json parsing for Erasure backend", isRight, erasureJSON)
                , ("Unknown Backend", "Verify server info json parsing for invalid backend", isLeft, invalidJSON)
                ]
    fsJSON = "[{\"error\":\"\",\"addr\":\"192.168.1.218:9000\",\"data\":{\"storage\":{\"Used\":20530,\"Backend\":{\"Type\":1,\"OnlineDisks\":0,\"OfflineDisks\":0,\"StandardSCData\":0,\"StandardSCParity\":0,\"RRSCData\":0,\"RRSCParity\":0,\"Sets\":null}},\"network\":{\"transferred\":808,\"received\":1160},\"http\":{\"totalHEADs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successHEADs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalGETs\":{\"count\":1,\"avgDuration\":\"0s\"},\"successGETs\":{\"count\":1,\"avgDuration\":\"0s\"},\"totalPUTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successPUTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalPOSTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successPOSTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalDELETEs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successDELETEs\":{\"count\":0,\"avgDuration\":\"0s\"}},\"server\":{\"uptime\":5992503019270,\"version\":\"DEVELOPMENT.GOGET\",\"commitID\":\"DEVELOPMENT.GOGET\",\"region\":\"\",\"sqsARN\":[]}}}]"

    erasureJSON = "[{\"error\":\"\",\"addr\":\"192.168.1.218:9000\",\"data\":{\"storage\":{\"Used\":83084,\"Backend\":{\"Type\":2,\"OnlineDisks\":4,\"OfflineDisks\":0,\"StandardSCData\":2,\"StandardSCParity\":2,\"RRSCData\":2,\"RRSCParity\":2,\"Sets\":[[{\"uuid\":\"16ec6f2c-9197-4787-904a-36bb2c2683f8\",\"endpoint\":\"/tmp/1\",\"state\":\"ok\"},{\"uuid\":\"4052e086-ef99-4aa5-ae2b-8e27559432f6\",\"endpoint\":\"/tmp/2\",\"state\":\"ok\"},{\"uuid\":\"d0639950-ddd3-45b0-93ca-fd86f5d79f72\",\"endpoint\":\"/tmp/3\",\"state\":\"ok\"},{\"uuid\":\"30ec68c0-37e1-4592-82c1-26b143c0ac10\",\"endpoint\":\"/tmp/4\",\"state\":\"ok\"}]]}},\"network\":{\"transferred\":404,\"received\":0},\"http\":{\"totalHEADs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successHEADs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalGETs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successGETs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalPUTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successPUTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalPOSTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successPOSTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalDELETEs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successDELETEs\":{\"count\":0,\"avgDuration\":\"0s\"}},\"server\":{\"uptime\":2738903073,\"version\":\"DEVELOPMENT.GOGET\",\"commitID\":\"DEVELOPMENT.GOGET\",\"region\":\"\",\"sqsARN\":[]}}}]"

    invalidJSON = "[{\"error\":\"\",\"addr\":\"192.168.1.218:9000\",\"data\":{\"storage\":{\"Used\":83084,\"Backend\":{\"Type\":42,\"OnlineDisks\":4,\"OfflineDisks\":0,\"StandardSCData\":2,\"StandardSCParity\":2,\"RRSCData\":2,\"RRSCParity\":2,\"Sets\":[[{\"uuid\":\"16ec6f2c-9197-4787-904a-36bb2c2683f8\",\"endpoint\":\"/tmp/1\",\"state\":\"ok\"},{\"uuid\":\"4052e086-ef99-4aa5-ae2b-8e27559432f6\",\"endpoint\":\"/tmp/2\",\"state\":\"ok\"},{\"uuid\":\"d0639950-ddd3-45b0-93ca-fd86f5d79f72\",\"endpoint\":\"/tmp/3\",\"state\":\"ok\"},{\"uuid\":\"30ec68c0-37e1-4592-82c1-26b143c0ac10\",\"endpoint\":\"/tmp/4\",\"state\":\"ok\"}]]}},\"network\":{\"transferred\":404,\"received\":0},\"http\":{\"totalHEADs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successHEADs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalGETs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successGETs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalPUTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successPUTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalPOSTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successPOSTs\":{\"count\":0,\"avgDuration\":\"0s\"},\"totalDELETEs\":{\"count\":0,\"avgDuration\":\"0s\"},\"successDELETEs\":{\"count\":0,\"avgDuration\":\"0s\"}},\"server\":{\"uptime\":2738903073,\"version\":\"DEVELOPMENT.GOGET\",\"commitID\":\"DEVELOPMENT.GOGET\",\"region\":\"\",\"sqsARN\":[]}}}]"

parseHealStatusTest :: TestTree
parseHealStatusTest = testGroup "Parse MinIO Admin API HealStatus JSON test" $
  map (\(tName, tDesc, tfn, tVal) -> testCase tName $ assertBool tDesc $
        tfn (eitherDecode tVal :: Either [Char] HealStatus)) testCases

  where
    testCases = [ ("Good", "Verify heal result item for erasure backend", isRight, erasureJSON')
                , ("Corrupted", "Verify heal result item for erasure backend", isLeft, invalidJSON')
                , ("Incorrect Value", "Verify heal result item for erasure backend", isLeft, invalidItemType)
                ]

    erasureJSON' = "{\"Summary\":\"finished\",\"StartTime\":\"2018-06-05T08:09:47.644465513Z\",\"NumDisks\":4,\"Settings\":{\"recursive\":false,\"dryRun\":false},\"Items\":[{\"resultId\":1,\"type\":\"metadata\",\"bucket\":\"\",\"object\":\"\",\"detail\":\"disk-format\",\"diskCount\":4,\"setCount\":1,\"before\":{\"drives\":[{\"uuid\":\"c3487166-b8a4-481a-b1e7-fb9b249e2500\",\"endpoint\":\"/tmp/1\",\"state\":\"ok\"},{\"uuid\":\"55a6e787-184f-4e4c-bf09-03dcada658a9\",\"endpoint\":\"/tmp/2\",\"state\":\"ok\"},{\"uuid\":\"f035d8c3-fca1-4407-b89c-38c2bcf4a641\",\"endpoint\":\"/tmp/3\",\"state\":\"ok\"},{\"uuid\":\"4f8b79d3-db90-4c1d-87c2-35a28b0d9a13\",\"endpoint\":\"/tmp/4\",\"state\":\"ok\"}]},\"after\":{\"drives\":[{\"uuid\":\"c3487166-b8a4-481a-b1e7-fb9b249e2500\",\"endpoint\":\"/tmp/1\",\"state\":\"ok\"},{\"uuid\":\"55a6e787-184f-4e4c-bf09-03dcada658a9\",\"endpoint\":\"/tmp/2\",\"state\":\"ok\"},{\"uuid\":\"f035d8c3-fca1-4407-b89c-38c2bcf4a641\",\"endpoint\":\"/tmp/3\",\"state\":\"ok\"},{\"uuid\":\"4f8b79d3-db90-4c1d-87c2-35a28b0d9a13\",\"endpoint\":\"/tmp/4\",\"state\":\"ok\"}]},\"objectSize\":0}]}"

    invalidJSON' = "{\"Summary\":\"finished\",\"StartTime\":\"2018-06-05T08:09:47.644465513Z\",\"NumDisks\":4,\"Settings\":{\"recursive\":false,\"dryRun\":false},\"Items\":[{\"resultId\":1,\"type\":\"metadata\",\"bucket\":\"\",\"object\":\"\",\"detail\":\"disk-format\",\"diskCount\":4,\"setCount\":1,\"before\":{\"drives\":[{\"uuid\":\"c3487166-b8a4-481a-b1e7-fb9b249e2500\",\"endpoint\":\"/tmp/1\",\"state\":\"ok\"},{\"uuid\":\"55a6e787-184f-4e4c-bf09-03dcada658a9\",\"endpoint\":\"/tmp/2\",\"state\":\"ok\"},{\"uuid\":\"f035d8c3-fca1-4407-b89c-38c2bcf4a641\",\"endpoint\":\"/tmp/3\",\"state\":\"ok\"},{\"uuid\":\"4f8b79d3-db90-4c1d-87c2-35a28b0d9a13\",\"endpoint\":\"/tmp/4\",\"state\":\"ok\"}]},\"after\":{\"drives\":[{\"uuid\":\"c3487166-b8a4-481a-b1e7-fb9b249e2500\",\"endpoint\":\"/tmp/1\",\"state\":\"ok\"},{\"uuid\":\"55a6e787-184f-4e4c-bf09-03dcada658a9\",\"endpoint\":\"/tmp/2\",\"state\":\"ok\"},{\"uuid\":\"f035d8c3-fca1-4407-b89c-38c2bcf4a641\",\"endpoint\":\"/tmp/3\",\"state\":\"ok\"},{\"uuid\":\"4f8b79d3-db90-4c1d-87c2-35a28b0d9a13\",\"endpoint\":\"/tmp/4\",\"state\":\"ok\"}]},\"objectSize\":0}]"

    invalidItemType = "{\"Summary\":\"finished\",\"StartTime\":\"2018-06-05T08:09:47.644465513Z\",\"NumDisks\":4,\"Settings\":{\"recursive\":false,\"dryRun\":false},\"Items\":[{\"resultId\":1,\"type\":\"hello\",\"bucket\":\"\",\"object\":\"\",\"detail\":\"disk-format\",\"diskCount\":4,\"setCount\":1,\"before\":{\"drives\":[{\"uuid\":\"c3487166-b8a4-481a-b1e7-fb9b249e2500\",\"endpoint\":\"/tmp/1\",\"state\":\"ok\"},{\"uuid\":\"55a6e787-184f-4e4c-bf09-03dcada658a9\",\"endpoint\":\"/tmp/2\",\"state\":\"ok\"},{\"uuid\":\"f035d8c3-fca1-4407-b89c-38c2bcf4a641\",\"endpoint\":\"/tmp/3\",\"state\":\"ok\"},{\"uuid\":\"4f8b79d3-db90-4c1d-87c2-35a28b0d9a13\",\"endpoint\":\"/tmp/4\",\"state\":\"ok\"}]},\"after\":{\"drives\":[{\"uuid\":\"c3487166-b8a4-481a-b1e7-fb9b249e2500\",\"endpoint\":\"/tmp/1\",\"state\":\"ok\"},{\"uuid\":\"55a6e787-184f-4e4c-bf09-03dcada658a9\",\"endpoint\":\"/tmp/2\",\"state\":\"ok\"},{\"uuid\":\"f035d8c3-fca1-4407-b89c-38c2bcf4a641\",\"endpoint\":\"/tmp/3\",\"state\":\"ok\"},{\"uuid\":\"4f8b79d3-db90-4c1d-87c2-35a28b0d9a13\",\"endpoint\":\"/tmp/4\",\"state\":\"ok\"}]},\"objectSize\":0}]}"

parseHealStartRespTest :: TestTree
parseHealStartRespTest = testGroup "Parse MinIO Admin API HealStartResp JSON test" $
  map (\(tName, tDesc, tfn, tVal) -> testCase tName $ assertBool tDesc $
        tfn (eitherDecode tVal :: Either [Char] HealStartResp)) testCases

  where
    testCases = [ ("Good", "Verify heal start response for erasure backend", isRight, hsrJSON)
                , ("Missing Token", "Verify heal start response for erasure backend", isLeft, missingTokenJSON)
                ]

    hsrJSON = "{\"clientToken\":\"3a3aca49-77dd-4b78-bba7-0978f119b23e\",\"clientAddress\":\"127.0.0.1\",\"startTime\":\"2018-06-05T08:09:47.644394493Z\"}"

    missingTokenJSON = "{\"clientAddress\":\"127.0.0.1\",\"startTime\":\"2018-06-05T08:09:47.644394493Z\"}"
