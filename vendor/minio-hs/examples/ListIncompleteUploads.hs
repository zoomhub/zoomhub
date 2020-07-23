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

import           Conduit
import           Prelude

-- | The following example uses minio's play server at
-- https://play.min.io.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

main :: IO ()
main = do
  let
    bucket = "test"

  -- Performs a recursive listing of incomplete uploads under bucket "test"
  -- on a local minio server.
  res <- runMinio minioPlayCI $
    runConduit $ listIncompleteUploads bucket Nothing True .| mapM_C (\v -> (liftIO $ print v))
  print res

  {-
    Following is the output of the above program on a local MinIO server.

    Right [UploadInfo { uiKey = "go1.6.2.linux-amd64.tar.gz"
                      , uiUploadId = "063eb592-bdd7-4a0c-be48-34fb3ceb63e2"
                      , uiInitTime = 2017-03-01 10:16:25.698 UTC
                      , uiSize = 17731794
                      }
          ]
  -}
