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

module Network.Minio.JsonParser
  (
    parseErrResponseJSON
  ) where

import           Data.Aeson           (FromJSON, eitherDecode, parseJSON,
                                       withObject, (.:))
import qualified Data.Text            as T

import           Lib.Prelude

import           Network.Minio.Errors

data AdminErrJSON = AdminErrJSON { aeCode    :: Text
                                 , aeMessage :: Text
                                 } deriving (Eq, Show)
instance FromJSON AdminErrJSON where
  parseJSON = withObject "AdminErrJSON" $ \v -> AdminErrJSON
    <$> v .: "Code"
    <*> v .: "Message"

parseErrResponseJSON :: (MonadIO m) => LByteString -> m ServiceErr
parseErrResponseJSON jsondata =
  case eitherDecode jsondata of
    Right aErr -> return $ toServiceErr (aeCode aErr) (aeMessage aErr)
    Left err   -> throwIO $ MErrVJsonParse $ T.pack err
