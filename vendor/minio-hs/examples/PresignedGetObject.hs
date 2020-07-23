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

import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Char8    as B
import           Data.CaseInsensitive     (original)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Text.Encoding       as E

-- | The following example uses minio's play server at
-- https://play.min.io.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

main :: IO ()
main = do
  let
    bucket = "my-bucket"
    object = "my-object"
    kb15 = 15*1024

    -- Set query parameter to modify content disposition response
    -- header
    queryParam = [("response-content-disposition",
                   Just "attachment; filename=\"your-filename.txt\"")]

  res <- runMinio minioPlayCI $ do
    liftIO $ B.putStrLn "Upload a file that we will fetch with a presigned URL..."
    putObject bucket object (CC.repeat "a") (Just kb15) def
    liftIO $ putStrLn $ "Done. Object created at: my-bucket/my-object"

    -- Extract Etag of uploaded object
    oi <- statObject bucket object
    let etag = oiETag oi

    -- Set header to add an if-match constraint - this makes sure
    -- the fetching fails if the object is changed on the server
    let headers = [("If-Match", E.encodeUtf8 etag)]

    -- Generate a URL with 7 days expiry time - note that the headers
    -- used above must be added to the request with the signed URL
    -- generated.
    url <- presignedGetObjectUrl "my-bucket" "my-object" (7*24*3600)
           queryParam headers

    return (headers, etag, url)

  case res of
    Left e -> putStrLn $ "presignedPutObject URL failed." ++ show e
    Right (headers, etag, url) -> do

      -- We generate a curl command to demonstrate usage of the signed
      -- URL.
      let
        hdrOpt (k, v) = B.concat ["-H '", original k, ": ", v, "'"]
        curlCmd = B.intercalate " " $
                  ["curl --fail"] ++ map hdrOpt headers ++
                  ["-o /tmp/myfile", B.concat ["'", url, "'"]]

      putStrLn $ "The following curl command would use the presigned " ++
        "URL to fetch the object and write it to \"/tmp/myfile\":"
      B.putStrLn curlCmd
