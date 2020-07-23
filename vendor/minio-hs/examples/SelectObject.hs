#!/usr/bin/env stack
-- stack --resolver lts-13.1 runghc --package minio-hs

--
-- MinIO Haskell SDK, (C) 2019 MinIO, Inc.
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

import qualified Conduit              as C
import           Control.Monad        (when)
import qualified Data.ByteString.Lazy as LB

import           Prelude

main :: IO ()
main = do
    let bucket = "selectbucket"
        object = "1.csv"
        content = "Name,Place,Temperature\n"
               <> "James,San Jose,76\n"
               <> "Alicia,San Leandro,88\n"
               <> "Mark,San Carlos,90\n"

    res <- runMinio minioPlayCI $ do

        exists <- bucketExists bucket
        when (not exists) $
            makeBucket bucket Nothing

        C.liftIO $ putStrLn "Uploading csv object"
        putObject bucket object (C.sourceLazy content) Nothing defaultPutObjectOptions

        let sr = selectRequest "Select * from s3object" defaultCsvInput defaultCsvOutput
        res <- selectObjectContent bucket object sr
        C.runConduit $ res C..| getPayloadBytes C..| C.stdoutC
    print res
