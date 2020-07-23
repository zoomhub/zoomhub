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

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.HashMap.Strict   as H
import qualified Data.Text.Encoding    as Enc
import qualified Data.Time             as Time

-- | The following example uses minio's play server at
-- https://play.min.io.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

main :: IO ()
main = do
  now <- Time.getCurrentTime
  let
    bucket = "my-bucket"
    object = "my-object"

    -- set an expiration time of 10 days
    expireTime = Time.addUTCTime (3600 * 24 * 10) now

    -- create a policy with expiration time and conditions - since the
    -- conditions are validated, newPostPolicy returns an Either value
    policyE = newPostPolicy expireTime
              [ -- set the object name condition
                ppCondKey "photos/my-object"
                -- set the bucket name condition
              , ppCondBucket "my-bucket"
                -- set the size range of object as 1B to 10MiB
              , ppCondContentLengthRange 1 (10*1024*1024)
                -- set content type as jpg image
              , ppCondContentType "image/jpeg"
                -- on success set the server response code to 200
              , ppCondSuccessActionStatus 200
              ]

  case policyE of
    Left err -> putStrLn $ show err
    Right policy -> do
      res <- runMinio minioPlayCI $ do
        (url, formData) <- presignedPostPolicy policy

        -- a curl command is output to demonstrate using the generated
        -- URL and form-data
        let
          formFn (k, v) = B.concat ["-F ", Enc.encodeUtf8 k, "=",
                                    "'", v, "'"]
          formOptions = B.intercalate " " $ map formFn $ H.toList formData


        return $ B.intercalate " " $
          ["curl", formOptions, "-F file=@/tmp/photo.jpg", url]

      case res of
        Left e -> putStrLn $ "post-policy error: " ++ (show e)
        Right cmd -> do
          putStrLn $ "Put a photo at /tmp/photo.jpg and run command:\n"

          -- print the generated curl command
          Char8.putStrLn cmd
