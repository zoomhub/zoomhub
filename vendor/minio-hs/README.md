# MinIO Client SDK for Haskell [![Build Status](https://travis-ci.org/minio/minio-hs.svg?branch=master)](https://travis-ci.org/minio/minio-hs)[![Hackage](https://img.shields.io/hackage/v/minio-hs.svg)](https://hackage.haskell.org/package/minio-hs)[![Slack](https://slack.min.io/slack?type=svg)](https://slack.min.io)

The MinIO Haskell Client SDK provides simple APIs to access [MinIO](https://min.io) and Amazon S3 compatible object storage server.

## Minimum Requirements

- The Haskell [stack](https://docs.haskellstack.org/en/stable/README/)

## Installation

```sh
git clone https://github.com/minio/minio-hs.git

cd minio-hs/

stack install
```

Tests can be run with:

```sh

stack test

```

A section of the tests use the remote MinIO Play server at
`https://play.min.io` by default. For library development,
using this remote server maybe slow. To run the tests against a
locally running MinIO live server at `http://localhost:9000`, just set
the environment `MINIO_LOCAL` to any value (and unset it to switch
back to Play).

Documentation can be locally built with:

```sh

stack haddock

```

## Quick-Start Example - File Uploader

### FileUploader.hs
``` haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.1 runghc --package minio-hs --package optparse-applicative --package filepath

--
-- MinIO Haskell SDK, (C) 2017-2019 MinIO, Inc.
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


{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Network.Minio

import           Data.Monoid           ((<>))
import           Data.Text             (pack)
import           Options.Applicative
import           System.FilePath.Posix
import           UnliftIO              (throwIO, try)

import           Prelude

-- | The following example uses minio's play server at
-- https://play.min.io.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

-- optparse-applicative package based command-line parsing.
fileNameArgs :: Parser FilePath
fileNameArgs = strArgument
               (metavar "FILENAME"
                <> help "Name of file to upload to AWS S3 or a MinIO server")

cmdParser = info
            (helper <*> fileNameArgs)
            (fullDesc
             <> progDesc "FileUploader"
             <> header
             "FileUploader - a simple file-uploader program using minio-hs")

main :: IO ()
main = do
  let bucket = "my-bucket"

  -- Parse command line argument
  filepath <- execParser cmdParser
  let object = pack $ takeBaseName filepath

  res <- runMinio minioPlayCI $ do
    -- Make a bucket; catch bucket already exists exception if thrown.
    bErr <- try $ makeBucket bucket Nothing
    case bErr of
      Left (MErrService BucketAlreadyOwnedByYou) -> return ()
      Left e                                     -> throwIO e
      Right _                                    -> return ()

    -- Upload filepath to bucket; object is derived from filepath.
    fPutObject bucket object filepath def

  case res of
    Left e   -> putStrLn $ "file upload failed due to " ++ (show e)
    Right () -> putStrLn "file upload succeeded."
```

### Run FileUploader

``` sh
./FileUploader.hs "path/to/my/file"

```

## Contribute

[Contributors Guide](https://github.com/minio/minio-hs/blob/master/CONTRIBUTING.md)
