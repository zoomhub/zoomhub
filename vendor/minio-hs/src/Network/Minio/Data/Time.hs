--
-- MinIO Haskell SDK, (C) 2017 MinIO, Inc.
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

module Network.Minio.Data.Time
  (
    awsTimeFormat
  , awsTimeFormatBS
  , awsDateFormat
  , awsDateFormatBS
  , awsParseTime
  , iso8601TimeFormat
  ) where


import           Data.ByteString.Char8 (pack)
import qualified Data.Time as Time

import           Lib.Prelude

awsTimeFormat :: UTCTime -> [Char]
awsTimeFormat = Time.formatTime Time.defaultTimeLocale "%Y%m%dT%H%M%SZ"

awsTimeFormatBS :: UTCTime -> ByteString
awsTimeFormatBS = pack . awsTimeFormat

awsDateFormat :: UTCTime -> [Char]
awsDateFormat = Time.formatTime Time.defaultTimeLocale "%Y%m%d"

awsDateFormatBS :: UTCTime -> ByteString
awsDateFormatBS = pack . awsDateFormat

awsParseTime :: [Char] -> Maybe UTCTime
awsParseTime = Time.parseTimeM False Time.defaultTimeLocale "%Y%m%dT%H%M%SZ"

iso8601TimeFormat :: UTCTime -> [Char]
iso8601TimeFormat = Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat $ Just "%T%QZ")
