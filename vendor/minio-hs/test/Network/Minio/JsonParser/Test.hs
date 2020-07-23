--
-- MinIO Haskell SDK, (C) 2018 MinIO, Inc.
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

module Network.Minio.JsonParser.Test
  (
    jsonParserTests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           UnliftIO                 (MonadUnliftIO)

import           Lib.Prelude

import           Network.Minio.Errors
import           Network.Minio.JsonParser

jsonParserTests :: TestTree
jsonParserTests = testGroup "JSON Parser Tests"
  [ testCase "Test parseErrResponseJSON" testParseErrResponseJSON
  ]

tryValidationErr :: (MonadUnliftIO m) => m a -> m (Either MErrV a)
tryValidationErr act = try act

assertValidationErr :: MErrV -> Assertion
assertValidationErr e = assertFailure $ "Failed due to validation error => " ++ show e

testParseErrResponseJSON :: Assertion
testParseErrResponseJSON = do
  -- 1. Test parsing of an invalid error json.
  parseResE <- tryValidationErr $ parseErrResponseJSON "ClearlyInvalidJSON"
  when (isRight parseResE) $
    assertFailure $ "Parsing should have failed => " ++ show parseResE

  forM_ cases $ \(jsondata, sErr) -> do
    parseErr <- tryValidationErr $ parseErrResponseJSON jsondata
    either assertValidationErr (@?= sErr) parseErr

  where
    cases =  [
      -- 2. Test parsing of a valid error json.
      ("{\"Code\":\"InvalidAccessKeyId\",\"Message\":\"The access key ID you provided does not exist in our records.\",\"Key\":\"\",\"BucketName\":\"\",\"Resource\":\"/minio/admin/v1/info\",\"RequestId\":\"3L137\",\"HostId\":\"3L137\"}",
       ServiceErr "InvalidAccessKeyId" "The access key ID you provided does not exist in our records."
      )
      ,
      -- 3. Test parsing of a valid, empty Resource.
      ("{\"Code\":\"SignatureDoesNotMatch\",\"Message\":\"The request signature we calculated does not match the signature you provided. Check your key and signing method.\",\"Key\":\"\",\"BucketName\":\"\",\"Resource\":\"/minio/admin/v1/info\",\"RequestId\":\"3L137\",\"HostId\":\"3L137\"}",
       ServiceErr "SignatureDoesNotMatch" "The request signature we calculated does not match the signature you provided. Check your key and signing method."
      )
      ]
