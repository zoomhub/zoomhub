#!/usr/bin/env stack
-- stack --resolver lts-11.1 runghc --package minio-hs

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

{-# LANGUAGE OverloadedStrings #-}
import           Network.Minio

import           Control.Monad.Catch (catchIf)
import           Prelude

-- | The following example uses minio's play server at
-- https://play.min.io.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

ignoreMinioErr :: ServiceErr -> Minio ()
ignoreMinioErr = return . const ()

main :: IO ()
main = do
  let
      bucket = "test"
      object = "obj"
      objectCopy = "obj-copy"
      localFile = "/etc/lsb-release"

  res1 <- runMinio minioPlayCI $ do
    -- 1. Make a bucket; Catch BucketAlreadyOwnedByYou exception.
    catchIf (== BucketAlreadyOwnedByYou) (makeBucket bucket Nothing) ignoreMinioErr

    -- 2. Upload a file to bucket/object.
    fPutObject bucket object localFile

    -- 3. Copy bucket/object to bucket/objectCopy.
    copyObject def {dstBucket = bucket, dstObject = objectCopy} def { srcBucket = bucket , srcObject = object }

  case res1 of
    Left e   -> putStrLn $ "copyObject failed." ++ show e
    Right () -> putStrLn "copyObject succeeded."
