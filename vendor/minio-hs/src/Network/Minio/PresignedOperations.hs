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

module Network.Minio.PresignedOperations
  ( UrlExpiry
  , makePresignedUrl
  , presignedPutObjectUrl
  , presignedGetObjectUrl
  , presignedHeadObjectUrl

  , PostPolicyCondition(..)
  , ppCondBucket
  , ppCondContentLengthRange
  , ppCondContentType
  , ppCondKey
  , ppCondKeyStartsWith
  , ppCondSuccessActionStatus
  , ppCondSuccessActionRedirect

  , PostPolicy(..)
  , PostPolicyError(..)
  , newPostPolicy
  , showPostPolicy
  , presignedPostPolicy
  ) where

import           Data.Aeson                ((.=))
import qualified Data.Aeson                as Json
import           Data.ByteString.Builder   (byteString, toLazyByteString)
import qualified Data.HashMap.Strict       as H
import qualified Data.Text                 as T
import qualified Data.Time                 as Time
import qualified Network.HTTP.Conduit      as NC
import qualified Network.HTTP.Types        as HT
import           Network.HTTP.Types.Header (hHost)

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Data.Time
import           Network.Minio.Errors
import           Network.Minio.Sign.V4

-- | Generate a presigned URL. This function allows for advanced usage
-- - for simple cases prefer the `presigned*Url` functions.
--
-- If region is Nothing, it is picked up from the connection
-- information (no check of bucket existence is performed).
--
-- All extra query parameters or headers are signed, and therefore are
-- required to be sent when the generated URL is actually used.
makePresignedUrl :: UrlExpiry -> HT.Method -> Maybe Bucket -> Maybe Object
                 -> Maybe Region -> HT.Query -> HT.RequestHeaders
                 -> Minio ByteString
makePresignedUrl expiry method bucket object region extraQuery extraHeaders = do
  when (expiry > 7*24*3600 || expiry < 0) $
    throwIO $ MErrVInvalidUrlExpiry expiry

  ci <- asks mcConnInfo

  let
    hostHeader = (hHost, getHostAddr ci)
    req = NC.defaultRequest {
          NC.method = method
        , NC.secure = connectIsSecure ci
        , NC.host = encodeUtf8 $ connectHost ci
        , NC.port = connectPort ci
        , NC.path = getS3Path bucket object
        , NC.requestHeaders = hostHeader : extraHeaders
        , NC.queryString = HT.renderQuery True extraQuery
        }
  ts <- liftIO Time.getCurrentTime

  let sp = SignParams (connectAccessKey ci) (connectSecretKey ci)
           ts region (Just expiry) Nothing

      signPairs = signV4 sp req

      qpToAdd = (fmap . fmap) Just signPairs
      queryStr = HT.renderQueryBuilder True
                 ((HT.parseQuery $ NC.queryString req) ++ qpToAdd)
      scheme = byteString $ bool "http://" "https://" $ connectIsSecure ci

  return $ toS $ toLazyByteString $ scheme
     <> byteString (getHostAddr ci)
     <> byteString (getS3Path bucket object)
     <> queryStr

-- | Generate a URL with authentication signature to PUT (upload) an
-- object. Any extra headers if passed, are signed, and so they are
-- required when the URL is used to upload data. This could be used,
-- for example, to set user-metadata on the object.
--
-- For a list of possible headers to pass, please refer to the PUT
-- object REST API AWS S3 documentation.
presignedPutObjectUrl :: Bucket -> Object -> UrlExpiry -> HT.RequestHeaders
                      -> Minio ByteString
presignedPutObjectUrl bucket object expirySeconds extraHeaders = do
  region <- asks (Just . connectRegion . mcConnInfo)
  makePresignedUrl expirySeconds HT.methodPut
      (Just bucket) (Just object) region [] extraHeaders

-- | Generate a URL with authentication signature to GET (download) an
-- object. All extra query parameters and headers passed here will be
-- signed and are required when the generated URL is used. Query
-- parameters could be used to change the response headers sent by the
-- server. Headers can be used to set Etag match conditions among
-- others.
--
-- For a list of possible request parameters and headers, please refer
-- to the GET object REST API AWS S3 documentation.
presignedGetObjectUrl :: Bucket -> Object -> UrlExpiry -> HT.Query
                      -> HT.RequestHeaders -> Minio ByteString
presignedGetObjectUrl bucket object expirySeconds extraQuery extraHeaders = do
  region <- asks (Just . connectRegion . mcConnInfo)
  makePresignedUrl expirySeconds HT.methodGet
      (Just bucket) (Just object) region extraQuery extraHeaders

-- | Generate a URL with authentication signature to make a HEAD
-- request on an object. This is used to fetch metadata about an
-- object. All extra headers passed here will be signed and are
-- required when the generated URL is used.
--
-- For a list of possible headers to pass, please refer to the HEAD
-- object REST API AWS S3 documentation.
presignedHeadObjectUrl :: Bucket -> Object -> UrlExpiry
                       -> HT.RequestHeaders -> Minio ByteString
presignedHeadObjectUrl bucket object expirySeconds extraHeaders = do
  region <- asks (Just . connectRegion . mcConnInfo)
  makePresignedUrl expirySeconds HT.methodHead
      (Just bucket) (Just object) region [] extraHeaders

-- | Represents individual conditions in a Post Policy document.
data PostPolicyCondition = PPCStartsWith Text Text
                         | PPCEquals Text Text
                         | PPCRange Text Int64 Int64
                         deriving (Show, Eq)

instance Json.ToJSON PostPolicyCondition where
  toJSON (PPCStartsWith k v) = Json.toJSON ["starts-with", k, v]
  toJSON (PPCEquals k v) = Json.object [k .= v]
  toJSON (PPCRange k minVal maxVal) =
    Json.toJSON [Json.toJSON k, Json.toJSON minVal, Json.toJSON maxVal]

  toEncoding (PPCStartsWith k v) = Json.foldable ["starts-with", k, v]
  toEncoding (PPCEquals k v) = Json.pairs (k .= v)
  toEncoding (PPCRange k minVal maxVal) =
    Json.foldable [Json.toJSON k, Json.toJSON minVal, Json.toJSON maxVal]

-- | A PostPolicy is required to perform uploads via browser forms.
data PostPolicy = PostPolicy {
    expiration :: UTCTime
  , conditions :: [PostPolicyCondition]
  } deriving (Show, Eq)

instance Json.ToJSON PostPolicy where
  toJSON (PostPolicy e c) =
    Json.object $ [ "expiration" .= iso8601TimeFormat e
                  , "conditions" .= c
                  ]
  toEncoding (PostPolicy e c) =
    Json.pairs ("expiration" .= iso8601TimeFormat e <> "conditions" .= c)

-- | Possible validation errors when creating a PostPolicy.
data PostPolicyError = PPEKeyNotSpecified
                     | PPEBucketNotSpecified
                     | PPEConditionKeyEmpty
                     | PPERangeInvalid
                     deriving (Eq, Show)

-- | Set the bucket name that the upload should use.
ppCondBucket :: Bucket -> PostPolicyCondition
ppCondBucket = PPCEquals "bucket"

-- | Set the content length range constraint with minimum and maximum
-- byte count values.
ppCondContentLengthRange :: Int64 -> Int64
                         -> PostPolicyCondition
ppCondContentLengthRange = PPCRange "content-length-range"

-- | Set the content-type header for the upload.
ppCondContentType :: Text -> PostPolicyCondition
ppCondContentType = PPCEquals "Content-Type"

-- | Set the object name constraint for the upload.
ppCondKey :: Object -> PostPolicyCondition
ppCondKey = PPCEquals "key"

-- | Set the object name prefix constraint for the upload.
ppCondKeyStartsWith :: Object -> PostPolicyCondition
ppCondKeyStartsWith = PPCStartsWith "key"

-- | Status code that the S3-server should send on a successful POST
-- upload
ppCondSuccessActionStatus :: Int -> PostPolicyCondition
ppCondSuccessActionStatus n =
  PPCEquals "success_action_status" (show n)

ppCondSuccessActionRedirect :: Text -> PostPolicyCondition
ppCondSuccessActionRedirect url =
  PPCEquals "success_action_redirect" url

-- | This function creates a PostPolicy after validating its
-- arguments.
newPostPolicy :: UTCTime -> [PostPolicyCondition]
              -> Either PostPolicyError PostPolicy
newPostPolicy expirationTime conds
  -- object name condition must be present
  | not $ any (keyEquals "key") conds =
      Left PPEKeyNotSpecified

  -- bucket name condition must be present
  | not $ any (keyEquals "bucket") conds =
      Left PPEBucketNotSpecified

  -- a condition with an empty key is invalid
  | any (keyEquals "") conds || any isEmptyRangeKey conds =
      Left PPEConditionKeyEmpty

  -- invalid range check
  | any isInvalidRange conds =
      Left PPERangeInvalid

  -- all good!
  | otherwise =
      return $ PostPolicy expirationTime conds

  where
    keyEquals k' (PPCStartsWith k _) = k == k'
    keyEquals k' (PPCEquals k _)     = k == k'
    keyEquals _ _                    = False

    isEmptyRangeKey (PPCRange k _ _) = k == ""
    isEmptyRangeKey _                = False

    isInvalidRange (PPCRange _ mi ma) = mi < 0 || mi > ma
    isInvalidRange _                  = False

-- | Convert Post Policy to a string (e.g. for printing).
showPostPolicy :: PostPolicy -> ByteString
showPostPolicy = toS . Json.encode

-- | Generate a presigned URL and POST policy to upload files via a
-- browser. On success, this function returns a URL and POST
-- form-data.
presignedPostPolicy :: PostPolicy
                    -> Minio (ByteString, H.HashMap Text ByteString)
presignedPostPolicy p = do
  ci <- asks mcConnInfo
  signTime <- liftIO $ Time.getCurrentTime

  let
    extraConditions =
      [ PPCEquals "x-amz-date" (toS $ awsTimeFormat signTime)
      , PPCEquals "x-amz-algorithm" "AWS4-HMAC-SHA256"
      , PPCEquals "x-amz-credential"
        (T.intercalate "/" [connectAccessKey ci,
                            decodeUtf8 $ mkScope signTime region])
      ]
    ppWithCreds = p {
      conditions = conditions p ++ extraConditions
      }
    sp = SignParams (connectAccessKey ci) (connectSecretKey ci)
         signTime (Just $ connectRegion ci) Nothing Nothing
    signData = signV4PostPolicy (showPostPolicy ppWithCreds) sp


    -- compute form-data
    mkPair (PPCStartsWith k v) = Just (k, v)
    mkPair (PPCEquals k v)     = Just (k, v)
    mkPair _                   = Nothing
    formFromPolicy = H.map toS $ H.fromList $ catMaybes $
                     mkPair <$> conditions ppWithCreds
    formData = formFromPolicy `H.union` signData

    -- compute POST upload URL
    bucket = H.lookupDefault "" "bucket" formData
    scheme = byteString $ bool "http://" "https://" $ connectIsSecure ci
    region = connectRegion ci

    url = toS $ toLazyByteString $ scheme <> byteString (getHostAddr ci) <>
          byteString "/" <> byteString (toS bucket) <> byteString "/"

  return (url, formData)
