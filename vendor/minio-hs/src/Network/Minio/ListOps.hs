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

module Network.Minio.ListOps where

import qualified Data.Conduit             as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List        as CL

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.S3API

-- | Represents a list output item - either an object or an object
-- prefix (i.e. a directory).
data ListItem = ListItemObject ObjectInfo
              | ListItemPrefix Text
    deriving (Show, Eq)

-- | @'listObjects' bucket prefix recurse@ lists objects in a bucket
-- similar to a file system tree traversal.
--
-- If @prefix@ is not 'Nothing', only items with the given prefix are
-- listed, otherwise items under the bucket are returned.
--
-- If @recurse@ is set to @True@ all directories under the prefix are
-- recursively traversed and only objects are returned.
--
-- If @recurse@ is set to @False@, objects and directories immediately
-- under the given prefix are returned (no recursive traversal is
-- performed).
listObjects :: Bucket -> Maybe Text -> Bool -> C.ConduitM () ListItem Minio ()
listObjects bucket prefix recurse = loop Nothing
  where
    loop :: Maybe Text -> C.ConduitM () ListItem Minio ()
    loop nextToken = do
      let
        delimiter = bool (Just "/") Nothing recurse

      res <- lift $ listObjects' bucket prefix nextToken delimiter Nothing
      CL.sourceList $ map ListItemObject $ lorObjects res
      unless recurse $
        CL.sourceList $ map ListItemPrefix $ lorCPrefixes res
      when (lorHasMore res) $
        loop (lorNextToken res)

-- | Lists objects - similar to @listObjects@, however uses the older
-- V1 AWS S3 API. Prefer @listObjects@ to this.
listObjectsV1 :: Bucket -> Maybe Text -> Bool
              -> C.ConduitM () ListItem Minio ()
listObjectsV1 bucket prefix recurse = loop Nothing
  where
    loop :: Maybe Text -> C.ConduitM () ListItem Minio ()
    loop nextMarker = do
      let
        delimiter = bool (Just "/") Nothing recurse

      res <- lift $ listObjectsV1' bucket prefix nextMarker delimiter Nothing
      CL.sourceList $ map ListItemObject $ lorObjects' res
      unless recurse $
        CL.sourceList $ map ListItemPrefix $ lorCPrefixes' res
      when (lorHasMore' res) $
        loop (lorNextMarker res)

-- | List incomplete uploads in a bucket matching the given prefix. If
-- recurse is set to True incomplete uploads for the given prefix are
-- recursively listed.
listIncompleteUploads :: Bucket -> Maybe Text -> Bool
                      -> C.ConduitM () UploadInfo Minio ()
listIncompleteUploads bucket prefix recurse = loop Nothing Nothing
  where
    loop :: Maybe Text -> Maybe Text -> C.ConduitM () UploadInfo Minio ()
    loop nextKeyMarker nextUploadIdMarker = do
      let
        delimiter = bool (Just "/") Nothing recurse

      res <- lift $ listIncompleteUploads' bucket prefix delimiter
             nextKeyMarker nextUploadIdMarker Nothing

      aggrSizes <- lift $ forM (lurUploads res) $ \(uKey, uId, _) -> do
            partInfos <- C.runConduit $ listIncompleteParts bucket uKey uId
                    C..| CC.sinkList
            return $ foldl (\sizeSofar p -> opiSize p + sizeSofar) 0 partInfos

      CL.sourceList $
        map (\((uKey, uId, uInitTime), size) ->
                UploadInfo uKey uId uInitTime size
            ) $ zip (lurUploads res) aggrSizes

      when (lurHasMore res) $
        loop (lurNextKey res) (lurNextUpload res)


-- | List object parts of an ongoing multipart upload for given
-- bucket, object and uploadId.
listIncompleteParts :: Bucket -> Object -> UploadId
                    -> C.ConduitM () ObjectPartInfo Minio ()
listIncompleteParts bucket object uploadId = loop Nothing
  where
    loop :: Maybe Text -> C.ConduitM () ObjectPartInfo Minio ()
    loop nextPartMarker = do
      res <- lift $ listIncompleteParts' bucket object uploadId Nothing
             nextPartMarker
      CL.sourceList $ lprParts res
      when (lprHasMore res) $
        loop (show <$> lprNextPart res)
