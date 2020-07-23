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

import qualified Data.Conduit.Combinators as CC

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
      object = "obj"
      localFile = "/etc/lsb-release"
      kb15 = 15 * 1024

  -- Eg 1. Upload a stream of repeating "a" using putObject with default options.
  res1 <- runMinio minioPlayCI $
    putObject bucket object (CC.repeat "a") (Just kb15) def
  case res1 of
    Left e   -> putStrLn $ "putObject failed." ++ show e
    Right () -> putStrLn "putObject succeeded."

  -- Eg 2. Upload a file using fPutObject with default options.
  res2 <- runMinio minioPlayCI $
    fPutObject bucket object localFile def
  case res2 of
    Left e   -> putStrLn $ "fPutObject failed." ++ show e
    Right () -> putStrLn "fPutObject succeeded."
