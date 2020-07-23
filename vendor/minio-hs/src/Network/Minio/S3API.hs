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

module Network.Minio.S3API
  (
    Region
  , getLocation

  -- * Listing buckets
  --------------------
  , getService

  -- * Listing objects
  --------------------
  , ListObjectsResult(..)
  , ListObjectsV1Result(..)
  , listObjects'
  , listObjectsV1'

  -- * Retrieving buckets
  , headBucket

  -- * Retrieving objects
  -----------------------
  , getObject'
  , headObject

  -- * Creating buckets and objects
  ---------------------------------
  , putBucket
  , ETag
  , maxSinglePutObjectSizeBytes
  , putObjectSingle'
  , putObjectSingle
  , copyObjectSingle

  -- * Multipart Upload APIs
  --------------------------
  , UploadId
  , PartTuple
  , Payload(..)
  , PartNumber
  , newMultipartUpload
  , putObjectPart
  , copyObjectPart
  , completeMultipartUpload
  , abortMultipartUpload
  , ListUploadsResult(..)
  , listIncompleteUploads'
  , ListPartsResult(..)
  , listIncompleteParts'

  -- * Deletion APIs
  --------------------------
  , deleteBucket
  , deleteObject

  -- * Presigned Operations
  -----------------------------
  , module Network.Minio.PresignedOperations

  -- ** Bucket Policies
  , getBucketPolicy
  , setBucketPolicy

  -- * Bucket Notifications
  -------------------------
  , Notification(..)
  , NotificationConfig(..)
  , Arn
  , Event(..)
  , Filter(..)
  , FilterKey(..)
  , FilterRules(..)
  , FilterRule(..)
  , getBucketNotification
  , putBucketNotification
  , removeAllBucketNotification
  ) where

import qualified Data.ByteString                   as BS
import qualified Data.Text                         as T
import qualified Network.HTTP.Conduit              as NC
import qualified Network.HTTP.Types                as HT
import           Network.HTTP.Types.Status         (status404)
import           UnliftIO                          (Handler (Handler))

import           Lib.Prelude

import           Network.Minio.API
import           Network.Minio.APICommon
import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.PresignedOperations
import           Network.Minio.Utils
import           Network.Minio.XmlGenerator
import           Network.Minio.XmlParser

-- | Fetch all buckets from the service.
getService :: Minio [BucketInfo]
getService = do
  resp <- executeRequest $ defaultS3ReqInfo {
      riNeedsLocation = False
    }
  parseListBuckets $ NC.responseBody resp

-- Parse headers from getObject and headObject calls.
parseGetObjectHeaders :: Object -> [HT.Header] -> Maybe ObjectInfo
parseGetObjectHeaders object headers =
    let metadataPairs = getMetadata headers
        userMetadata = getUserMetadataMap metadataPairs
        metadata = getNonUserMetadataMap metadataPairs
    in ObjectInfo <$> Just object
                  <*> getLastModifiedHeader headers
                  <*> getETagHeader headers
                  <*> getContentLength headers
                  <*> Just userMetadata
                  <*> Just metadata

-- | GET an object from the service and return parsed ObjectInfo and a
-- conduit source for the object content
getObject' :: Bucket -> Object -> HT.Query -> [HT.Header]
           -> Minio GetObjectResponse
getObject' bucket object queryParams headers = do
    resp <- mkStreamRequest reqInfo
    let objInfoMaybe = parseGetObjectHeaders object $ NC.responseHeaders resp
    objInfo <- maybe (throwIO MErrVInvalidObjectInfoResponse) return
               objInfoMaybe
    return $ GetObjectResponse { gorObjectInfo = objInfo
                               , gorObjectStream = NC.responseBody resp
                               }
  where
    reqInfo = defaultS3ReqInfo { riBucket = Just bucket
                               , riObject = Just object
                               , riQueryParams = queryParams
                               , riHeaders = headers
                               }

-- | Creates a bucket via a PUT bucket call.
putBucket :: Bucket -> Region -> Minio ()
putBucket bucket location = do
  ns <- asks getSvcNamespace
  void $ executeRequest $
    defaultS3ReqInfo { riMethod = HT.methodPut
        , riBucket = Just bucket
        , riPayload = PayloadBS $ mkCreateBucketConfig ns location
        , riNeedsLocation = False
        }

-- | Single PUT object size.
maxSinglePutObjectSizeBytes :: Int64
maxSinglePutObjectSizeBytes = 5 * 1024 * 1024 * 1024

-- | PUT an object into the service. This function performs a single
-- PUT object call and uses a strict ByteString as the object
-- data. `putObjectSingle` is preferable as the object data will not
-- be resident in memory.
putObjectSingle' :: Bucket -> Object -> [HT.Header] -> ByteString -> Minio ETag
putObjectSingle' bucket object headers bs = do
  let size = fromIntegral (BS.length bs)
  -- check length is within single PUT object size.
  when (size > maxSinglePutObjectSizeBytes) $
    throwIO $ MErrVSinglePUTSizeExceeded size

  let payload = mkStreamingPayload $ PayloadBS bs
  resp <- executeRequest $
          defaultS3ReqInfo { riMethod = HT.methodPut
              , riBucket = Just bucket
              , riObject = Just object
              , riHeaders = headers
              , riPayload = payload
              }

  let rheaders = NC.responseHeaders resp
      etag = getETagHeader rheaders
  maybe
    (throwIO MErrVETagHeaderNotFound)
    return etag

-- | PUT an object into the service. This function performs a single
-- PUT object call, and so can only transfer objects upto 5GiB.
putObjectSingle :: Bucket -> Object -> [HT.Header] -> Handle -> Int64
                -> Int64 -> Minio ETag
putObjectSingle bucket object headers h offset size = do
  -- check length is within single PUT object size.
  when (size > maxSinglePutObjectSizeBytes) $
    throwIO $ MErrVSinglePUTSizeExceeded size

  -- content-length header is automatically set by library.
  let payload = mkStreamingPayload $ PayloadH h offset size
  resp <- executeRequest $
          defaultS3ReqInfo { riMethod = HT.methodPut
                           , riBucket = Just bucket
                           , riObject = Just object
                           , riHeaders = headers
                           , riPayload = payload
                           }

  let rheaders = NC.responseHeaders resp
      etag = getETagHeader rheaders
  maybe
    (throwIO MErrVETagHeaderNotFound)
    return etag

-- | List objects in a bucket matching prefix up to delimiter,
-- starting from nextMarker.
listObjectsV1' :: Bucket -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int
            -> Minio ListObjectsV1Result
listObjectsV1' bucket prefix nextMarker delimiter maxKeys = do
  resp <- executeRequest $ defaultS3ReqInfo { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riQueryParams = mkOptionalParams params
                               }
  parseListObjectsV1Response $ NC.responseBody resp
  where
    params = [
        ("marker", nextMarker)
      , ("prefix", prefix)
      , ("delimiter", delimiter)
      , ("max-keys", show <$> maxKeys)
      ]

-- | List objects in a bucket matching prefix up to delimiter,
-- starting from nextToken.
listObjects' :: Bucket -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int
            -> Minio ListObjectsResult
listObjects' bucket prefix nextToken delimiter maxKeys = do
  resp <- executeRequest $ defaultS3ReqInfo { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riQueryParams = mkOptionalParams params
                               }
  parseListObjectsResponse $ NC.responseBody resp
  where
    params = [
        ("list-type", Just "2")
      , ("continuation_token", nextToken)
      , ("prefix", prefix)
      , ("delimiter", delimiter)
      , ("max-keys", show <$> maxKeys)
      ]

-- | DELETE a bucket from the service.
deleteBucket :: Bucket -> Minio ()
deleteBucket bucket = void $
  executeRequest $
    defaultS3ReqInfo { riMethod = HT.methodDelete
        , riBucket = Just bucket
        }

-- | DELETE an object from the service.
deleteObject :: Bucket -> Object -> Minio ()
deleteObject bucket object = void $
  executeRequest $
    defaultS3ReqInfo { riMethod = HT.methodDelete
        , riBucket = Just bucket
        , riObject = Just object
        }

-- | Create a new multipart upload.
newMultipartUpload :: Bucket -> Object -> [HT.Header] -> Minio UploadId
newMultipartUpload bucket object headers = do
  resp <- executeRequest $ defaultS3ReqInfo { riMethod = HT.methodPost
                               , riBucket = Just bucket
                               , riObject = Just object
                               , riQueryParams = [("uploads", Nothing)]
                               , riHeaders = headers
                               }
  parseNewMultipartUpload $ NC.responseBody resp

-- | PUT a part of an object as part of a multipart upload.
putObjectPart :: Bucket -> Object -> UploadId -> PartNumber -> [HT.Header]
              -> Payload -> Minio PartTuple
putObjectPart bucket object uploadId partNumber headers payload = do
  -- transform payload to conduit to enable streaming signature
  let payload' = mkStreamingPayload payload
  resp <- executeRequest $
          defaultS3ReqInfo { riMethod = HT.methodPut
                           , riBucket = Just bucket
                           , riObject = Just object
                           , riQueryParams = mkOptionalParams params
                           , riHeaders = headers
                           , riPayload = payload'
                           }
  let rheaders = NC.responseHeaders resp
      etag = getETagHeader rheaders
  maybe
    (throwIO MErrVETagHeaderNotFound)
    (return . (partNumber, )) etag
  where
    params = [
        ("uploadId", Just uploadId)
      , ("partNumber", Just $ show partNumber)
      ]

srcInfoToHeaders :: SourceInfo -> [HT.Header]
srcInfoToHeaders srcInfo = ("x-amz-copy-source",
                            toS $ T.concat ["/", srcBucket srcInfo,
                                            "/", srcObject srcInfo]
                           ) : rangeHdr ++ zip names values
  where
    names = ["x-amz-copy-source-if-match", "x-amz-copy-source-if-none-match",
             "x-amz-copy-source-if-unmodified-since",
             "x-amz-copy-source-if-modified-since"]
    values = mapMaybe (fmap encodeUtf8 . (srcInfo &))
             [srcIfMatch, srcIfNoneMatch,
              fmap formatRFC1123 . srcIfUnmodifiedSince,
              fmap formatRFC1123 . srcIfModifiedSince]
    rangeHdr = maybe [] (\a -> [("x-amz-copy-source-range", HT.renderByteRanges [a])])
               $ toByteRange <$> srcRange srcInfo
    toByteRange :: (Int64, Int64) -> HT.ByteRange
    toByteRange (x, y) = HT.ByteRangeFromTo (fromIntegral x) (fromIntegral y)

-- | Performs server-side copy of an object or part of an object as an
-- upload part of an ongoing multi-part upload.
copyObjectPart :: DestinationInfo -> SourceInfo -> UploadId
               -> PartNumber -> [HT.Header] -> Minio (ETag, UTCTime)
copyObjectPart dstInfo srcInfo uploadId partNumber headers = do
  resp <- executeRequest $
          defaultS3ReqInfo { riMethod = HT.methodPut
              , riBucket = Just $ dstBucket dstInfo
              , riObject = Just $ dstObject dstInfo
              , riQueryParams = mkOptionalParams params
              , riHeaders = headers ++ srcInfoToHeaders srcInfo
              }

  parseCopyObjectResponse $ NC.responseBody resp
  where
    params = [
        ("uploadId", Just uploadId)
      , ("partNumber", Just $ show partNumber)
      ]

-- | Performs server-side copy of an object that is upto 5GiB in
-- size. If the object is greater than 5GiB, this function throws the
-- error returned by the server.
copyObjectSingle :: Bucket -> Object -> SourceInfo -> [HT.Header]
                 -> Minio (ETag, UTCTime)
copyObjectSingle bucket object srcInfo headers = do
  -- validate that srcRange is Nothing for this API.
  when (isJust $ srcRange srcInfo) $
    throwIO MErrVCopyObjSingleNoRangeAccepted
  resp <- executeRequest $
          defaultS3ReqInfo { riMethod = HT.methodPut
              , riBucket = Just bucket
              , riObject = Just object
              , riHeaders = headers ++ srcInfoToHeaders srcInfo
              }
  parseCopyObjectResponse $ NC.responseBody resp

-- | Complete a multipart upload.
completeMultipartUpload :: Bucket -> Object -> UploadId -> [PartTuple]
                        -> Minio ETag
completeMultipartUpload bucket object uploadId partTuple = do
  resp <- executeRequest $
          defaultS3ReqInfo { riMethod = HT.methodPost
              , riBucket = Just bucket
              , riObject = Just object
              , riQueryParams = mkOptionalParams params
              , riPayload = PayloadBS $
                            mkCompleteMultipartUploadRequest partTuple
              }
  parseCompleteMultipartUploadResponse $ NC.responseBody resp
  where
    params = [("uploadId", Just uploadId)]

-- | Abort a multipart upload.
abortMultipartUpload :: Bucket -> Object -> UploadId -> Minio ()
abortMultipartUpload bucket object uploadId = void $
  executeRequest $ defaultS3ReqInfo { riMethod = HT.methodDelete
                              , riBucket = Just bucket
                              , riObject = Just object
                              , riQueryParams = mkOptionalParams params
                              }
  where
    params = [("uploadId", Just uploadId)]

-- | List incomplete multipart uploads.
listIncompleteUploads' :: Bucket -> Maybe Text -> Maybe Text -> Maybe Text
                       -> Maybe Text -> Maybe Int -> Minio ListUploadsResult
listIncompleteUploads' bucket prefix delimiter keyMarker uploadIdMarker maxKeys = do
  resp <- executeRequest $ defaultS3ReqInfo { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riQueryParams = params
                               }
  parseListUploadsResponse $ NC.responseBody resp
  where
    -- build query params
    params = ("uploads", Nothing) : mkOptionalParams
             [ ("prefix", prefix)
             , ("delimiter", delimiter)
             , ("key-marker", keyMarker)
             , ("upload-id-marker", uploadIdMarker)
             , ("max-uploads", show <$> maxKeys)
             ]


-- | List parts of an ongoing multipart upload.
listIncompleteParts' :: Bucket -> Object -> UploadId -> Maybe Text
                     -> Maybe Text -> Minio ListPartsResult
listIncompleteParts' bucket object uploadId maxParts partNumMarker = do
  resp <- executeRequest $ defaultS3ReqInfo { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riObject = Just object
                               , riQueryParams = mkOptionalParams params
                               }
  parseListPartsResponse $ NC.responseBody resp
  where
    -- build optional query params
    params = [
        ("uploadId", Just uploadId)
      , ("part-number-marker", partNumMarker)
      , ("max-parts", maxParts)
      ]

-- | Get metadata of an object.
headObject :: Bucket -> Object -> [HT.Header] -> Minio ObjectInfo
headObject bucket object reqHeaders = do
  resp <- executeRequest $ defaultS3ReqInfo { riMethod = HT.methodHead
                                            , riBucket = Just bucket
                                            , riObject = Just object
                                            , riHeaders = reqHeaders
                                            }

  maybe (throwIO MErrVInvalidObjectInfoResponse) return $
    parseGetObjectHeaders object $ NC.responseHeaders resp


-- | Query the object store if a given bucket exists.
headBucket :: Bucket -> Minio Bool
headBucket bucket = headBucketEx `catches`
                    [ Handler handleNoSuchBucket
                    , Handler handleStatus404
                    ]

  where
    handleNoSuchBucket :: ServiceErr -> Minio Bool
    handleNoSuchBucket e | e == NoSuchBucket = return False
                         | otherwise = throwIO e

    handleStatus404 :: NC.HttpException -> Minio Bool
    handleStatus404 e@(NC.HttpExceptionRequest _ (NC.StatusCodeException res _)) =
      if NC.responseStatus res == status404
      then return False
      else throwIO e
    handleStatus404 e = throwIO e

    headBucketEx = do
      resp <- executeRequest $ defaultS3ReqInfo { riMethod = HT.methodHead
                                   , riBucket = Just bucket
                                   }
      return $ NC.responseStatus resp == HT.ok200

-- | Set the notification configuration on a bucket.
putBucketNotification :: Bucket -> Notification -> Minio ()
putBucketNotification bucket ncfg = do
  ns <- asks getSvcNamespace
  void $ executeRequest $ defaultS3ReqInfo { riMethod = HT.methodPut
                              , riBucket = Just bucket
                              , riQueryParams = [("notification", Nothing)]
                              , riPayload = PayloadBS $
                                            mkPutNotificationRequest ns ncfg
                              }

-- | Retrieve the notification configuration on a bucket.
getBucketNotification :: Bucket -> Minio Notification
getBucketNotification bucket = do
  resp <- executeRequest $ defaultS3ReqInfo { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riQueryParams = [("notification", Nothing)]
                               }
  parseNotification $ NC.responseBody resp

-- | Remove all notifications configured on a bucket.
removeAllBucketNotification :: Bucket -> Minio ()
removeAllBucketNotification = flip putBucketNotification defaultNotification

-- | Fetch the policy if any on a bucket.
getBucketPolicy :: Bucket -> Minio Text
getBucketPolicy bucket = do
  resp <- executeRequest $ defaultS3ReqInfo { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riQueryParams = [("policy", Nothing)]
                               }
  return $ toS $ NC.responseBody resp

-- | Set a new policy on a bucket.
-- As a special condition if the policy is empty
-- then we treat it as policy DELETE operation.
setBucketPolicy :: Bucket -> Text -> Minio ()
setBucketPolicy bucket policy = do
  if T.null policy
    then deleteBucketPolicy bucket
    else putBucketPolicy bucket policy

-- | Save a new policy on a bucket.
putBucketPolicy :: Bucket -> Text -> Minio()
putBucketPolicy bucket policy = do
  void $ executeRequest $ defaultS3ReqInfo { riMethod = HT.methodPut
                              , riBucket = Just bucket
                              , riQueryParams = [("policy", Nothing)]
                              , riPayload = PayloadBS $ encodeUtf8 policy
                              }

-- | Delete any policy set on a bucket.
deleteBucketPolicy :: Bucket -> Minio()
deleteBucketPolicy bucket = do
  void $ executeRequest $ defaultS3ReqInfo { riMethod = HT.methodDelete
                              , riBucket = Just bucket
                              , riQueryParams = [("policy", Nothing)]
                              }
