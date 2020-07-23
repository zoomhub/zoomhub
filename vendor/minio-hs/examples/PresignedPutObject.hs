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

import qualified Data.ByteString.Char8 as B
import           Data.CaseInsensitive  (original)

-- | The following example uses minio's play server at
-- https://play.min.io.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

main :: IO ()
main = do
  let
    -- Use headers to set user-metadata - note that this header will
    -- need to be set when the URL is used to make an upload.
    headers = [("x-amz-meta-url-creator",
                "minio-hs-presigned-put-example")]
  res <- runMinio minioPlayCI $ do

    -- generate a URL with 7 days expiry time
    presignedPutObjectUrl "my-bucket" "my-object" (7*24*3600) headers

  case res of
    Left e -> putStrLn $ "presignedPutObject URL failed." ++ show e
    Right url -> do

      -- We generate a curl command to demonstrate usage of the signed
      -- URL.
      let
        hdrOpt (k, v) = B.concat ["-H '", original k, ": ", v, "'"]
        curlCmd = B.intercalate " " $
                  ["curl "] ++ map hdrOpt headers ++
                  ["-T /tmp/myfile", B.concat ["'", url, "'"]]

      putStrLn $ "The following curl command would use the presigned " ++
        "URL to upload the file at \"/tmp/myfile\":"
      B.putStrLn curlCmd
