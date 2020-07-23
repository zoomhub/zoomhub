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

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Network.Minio.Data where

import qualified Conduit                      as C
import qualified Control.Concurrent.MVar      as M
import           Control.Monad.IO.Unlift      (UnliftIO (..), askUnliftIO,
                                               withUnliftIO)
import           Control.Monad.Trans.Resource
import qualified Data.Aeson                   as A
import qualified Data.ByteArray               as BA
import qualified Data.ByteString              as B
import           Data.CaseInsensitive         (mk)
import qualified Data.HashMap.Strict          as H
import qualified Data.Ini                     as Ini
import           Data.String                  (IsString (..))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Time                    (defaultTimeLocale, formatTime)
import           GHC.Show                     (Show (show))
import qualified Network.Connection           as Conn
import           Network.HTTP.Client          (defaultManagerSettings)
import qualified Network.HTTP.Client.TLS      as TLS
import qualified Network.HTTP.Conduit         as NC
import           Network.HTTP.Types           (ByteRange, Header, Method, Query,
                                               hRange)
import qualified Network.HTTP.Types           as HT
import           Network.Minio.Errors
import           System.Directory             (doesFileExist, getHomeDirectory)
import qualified System.Environment           as Env
import           System.FilePath.Posix        (combine)
import           Text.XML
import qualified UnliftIO                     as U

import           Lib.Prelude
import           Network.Minio.Data.Crypto

-- | max obj size is 5TiB
maxObjectSize :: Int64
maxObjectSize = 5 * 1024 * 1024 * oneMiB

-- | minimum size of parts used in multipart operations.
minPartSize :: Int64
minPartSize = 64 * oneMiB

oneMiB :: Int64
oneMiB = 1024 * 1024

-- | maximum number of parts that can be uploaded for a single object.
maxMultipartParts :: Int64
maxMultipartParts = 10000

-- TODO: Add a type which provides typed constants for region.  this
-- type should have a IsString instance to infer the appropriate
-- constant.
-- | awsRegionMap - library constant
awsRegionMap :: H.HashMap Text Text
awsRegionMap = H.fromList [
      ("us-east-1", "s3.amazonaws.com")
    , ("us-east-2", "s3-us-east-2.amazonaws.com")
    , ("us-west-1", "s3-us-west-1.amazonaws.com")
    , ("us-east-2", "s3-us-west-2.amazonaws.com")
    , ("ca-central-1", "s3-ca-central-1.amazonaws.com")
    , ("ap-south-1", "s3-ap-south-1.amazonaws.com")
    , ("ap-northeast-1", "s3-ap-northeast-1.amazonaws.com")
    , ("ap-northeast-2", "s3-ap-northeast-2.amazonaws.com")
    , ("ap-southeast-1", "s3-ap-southeast-1.amazonaws.com")
    , ("ap-southeast-2", "s3-ap-southeast-2.amazonaws.com")
    , ("eu-west-1", "s3-eu-west-1.amazonaws.com")
    , ("eu-west-2", "s3-eu-west-2.amazonaws.com")
    , ("eu-central-1", "s3-eu-central-1.amazonaws.com")
    , ("sa-east-1", "s3-sa-east-1.amazonaws.com")
  ]

-- | Connection Info data type. To create a 'ConnectInfo' value,
-- enable the @OverloadedStrings@ language extension and use the
-- `IsString` instance to provide a URL, for example:
--
-- > let c :: ConnectInfo = "https://play.min.io"
data ConnectInfo =
    ConnectInfo { connectHost                     :: Text
                , connectPort                     :: Int
                , connectAccessKey                :: Text
                , connectSecretKey                :: Text
                , connectIsSecure                 :: Bool
                , connectRegion                   :: Region
                , connectAutoDiscoverRegion       :: Bool
                , connectDisableTLSCertValidation :: Bool
                } deriving (Eq, Show)

instance IsString ConnectInfo where
    fromString str =
        let req = NC.parseRequest_ str
        in ConnectInfo
           { connectHost = TE.decodeUtf8 $ NC.host req
           , connectPort = NC.port req
           , connectAccessKey = ""
           , connectSecretKey = ""
           , connectIsSecure = NC.secure req
           , connectRegion = ""
           , connectAutoDiscoverRegion = True
           , connectDisableTLSCertValidation = False
           }

-- | Contains access key and secret key to access object storage.
data Credentials = Credentials { cAccessKey :: Text
                               , cSecretKey :: Text
                               } deriving (Eq, Show)

-- | A Provider is an action that may return Credentials. Providers
-- may be chained together using 'findFirst'.
type Provider = IO (Maybe Credentials)

-- | Combines the given list of providers, by calling each one in
-- order until Credentials are found.
findFirst :: [Provider] -> Provider
findFirst [] = return Nothing
findFirst (f:fs) = do c <- f
                      maybe (findFirst fs) (return . Just) c

-- | This Provider loads `Credentials` from @~\/.aws\/credentials@
fromAWSConfigFile :: Provider
fromAWSConfigFile = do
    credsE <- runExceptT $ do
        homeDir <- lift $ getHomeDirectory
        let awsCredsFile =  homeDir `combine` ".aws" `combine` "credentials"
        fileExists <- lift $ doesFileExist awsCredsFile
        bool (throwE "FileNotFound") (return ()) fileExists
        ini <- ExceptT $ Ini.readIniFile awsCredsFile
        akey <- ExceptT $ return
                $ Ini.lookupValue "default" "aws_access_key_id" ini
        skey <- ExceptT $ return
                $ Ini.lookupValue "default" "aws_secret_access_key" ini
        return $ Credentials akey skey
    return $ hush credsE

-- | This Provider loads `Credentials` from @AWS_ACCESS_KEY_ID@ and
-- @AWS_SECRET_ACCESS_KEY@ environment variables.
fromAWSEnv :: Provider
fromAWSEnv = runMaybeT $ do
        akey <- MaybeT $ Env.lookupEnv "AWS_ACCESS_KEY_ID"
        skey <- MaybeT $ Env.lookupEnv "AWS_SECRET_ACCESS_KEY"
        return $ Credentials (T.pack akey) (T.pack skey)

-- | This Provider loads `Credentials` from @MINIO_ACCESS_KEY@ and
-- @MINIO_SECRET_KEY@ environment variables.
fromMinioEnv :: Provider
fromMinioEnv = runMaybeT $ do
    akey <- MaybeT $ Env.lookupEnv "MINIO_ACCESS_KEY"
    skey <- MaybeT $ Env.lookupEnv "MINIO_SECRET_KEY"
    return $ Credentials (T.pack akey) (T.pack skey)

-- | setCredsFrom retrieves access credentials from the first
-- `Provider` form the given list that succeeds and sets it in the
-- `ConnectInfo`.
setCredsFrom :: [Provider] -> ConnectInfo -> IO ConnectInfo
setCredsFrom ps ci = do pMay <- findFirst ps
                        maybe
                          (throwIO MErrVMissingCredentials)
                          (return . (flip setCreds ci))
                          pMay

-- | setCreds sets the given `Credentials` in the `ConnectInfo`.
setCreds :: Credentials -> ConnectInfo -> ConnectInfo
setCreds (Credentials accessKey secretKey) connInfo =
    connInfo { connectAccessKey = accessKey
             , connectSecretKey = secretKey
             }

-- | Set the S3 region parameter in the `ConnectInfo`
setRegion :: Region -> ConnectInfo -> ConnectInfo
setRegion r connInfo = connInfo { connectRegion = r
                                , connectAutoDiscoverRegion = False
                                }

-- | Check if the connection to object storage server is secure
-- (i.e. uses TLS)
isConnectInfoSecure :: ConnectInfo -> Bool
isConnectInfoSecure = connectIsSecure

-- | Disable TLS certificate validation completely! This makes TLS
-- insecure! Use only for testing with self-signed or temporary
-- certificates. Note that this option has no effect, if you provide
-- your own Manager in `mkMinioConn`.
disableTLSCertValidation :: ConnectInfo -> ConnectInfo
disableTLSCertValidation c = c { connectDisableTLSCertValidation = True }

getHostAddr :: ConnectInfo -> ByteString
getHostAddr ci = if | port == 80 || port == 443 -> toS host
                    | otherwise -> toS $
                                   T.concat [ host, ":" , Lib.Prelude.show port]
  where
    port = connectPort ci
    host = connectHost ci


-- | Default Google Compute Storage ConnectInfo. Works only for
-- "Simple Migration" use-case with interoperability mode enabled on
-- GCP console. For more information -
-- https://cloud.google.com/storage/docs/migrating
--
-- Credentials should be supplied before use.
gcsCI :: ConnectInfo
gcsCI = setRegion "us"
        "https://storage.googleapis.com"


-- | Default AWS S3 ConnectInfo. Connects to "us-east-1". Credentials
-- should be supplied before use.
awsCI :: ConnectInfo
awsCI = "https://s3.amazonaws.com"


-- | <https://play.min.io MinIO Play Server>
-- ConnectInfo. Credentials are already filled in.
minioPlayCI :: ConnectInfo
minioPlayCI = let playCreds = Credentials "Q3AM3UQ867SPQQA43P2F" "zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG"
              in setCreds playCreds
                 $ setRegion "us-east-1"
                 "https://play.min.io"

-- |
-- Represents a bucket in the object store
type Bucket = Text

-- |
-- Represents an object name
type Object = Text

-- | Represents a region
type Region = Text

-- | A type alias to represent an Entity-Tag returned by S3-compatible APIs.
type ETag = Text

-- | Data type to represent an object encryption key. Create one using
-- the `mkSSECKey` function.
newtype SSECKey = SSECKey BA.ScrubbedBytes
                deriving (Eq, Show)

-- | Validates that the given ByteString is 32 bytes long and creates
-- an encryption key.
mkSSECKey :: MonadThrow m => ByteString -> m SSECKey
mkSSECKey keyBytes | B.length keyBytes /= 32 =
                         throwM MErrVInvalidEncryptionKeyLength
                   | otherwise =
                         return $ SSECKey $ BA.convert keyBytes

-- | Data type to represent Server-Side-Encryption settings
data SSE where
    -- | Specifies SSE S3 encryption - server manages encryption keys
    SSE :: SSE
    -- | Specifies that KMS service should be used. The first argument
    -- to the constructor is the Key Id to be used by the server (if
    -- not specified, the default KMS key id is used). The second
    -- argument is the optional KMS context that must have a
    -- `A.ToJSON` instance - please refer to the AWS S3 documentation
    -- for detailed information.
    SSEKMS :: A.ToJSON a => Maybe ByteString -> Maybe a -> SSE
    -- | Specifies server-side encryption with customer provided
    -- key. The argument is the encryption key to be used.
    SSEC :: SSECKey -> SSE

toPutObjectHeaders :: SSE -> [HT.Header]
toPutObjectHeaders sseArg =
    let sseHeader = "x-amz-server-side-encryption"
        sseKmsIdHeader = sseHeader <> "-aws-kms-key-id"
        sseKmsContextHeader = sseHeader <> "-context"
        ssecAlgo = sseHeader <> "-customer-algorithm"
        ssecKey = sseHeader <> "-customer-key"
        ssecKeyMD5 = ssecKey <> "-MD5"

    in case sseArg of
      SSE -> [(sseHeader, "AES256")]

      SSEKMS keyIdMay ctxMay ->
          [(sseHeader, "aws:kms")] ++
          maybe [] (\k -> [(sseKmsIdHeader, k)]) keyIdMay ++
          maybe [] (\k -> [(sseKmsContextHeader, toS $ A.encode k)]) ctxMay

      SSEC (SSECKey sb) ->
          [(ssecAlgo, "AES256"),
           (ssecKey, encodeToBase64 sb),
           (ssecKeyMD5, hashMD5ToBase64 sb)]

-- | Data type for options in PutObject call.  Start with the empty
-- `defaultPutObjectOptions` and use various the various poo*
-- accessors.
data PutObjectOptions = PutObjectOptions {
  -- | Set a standard MIME type describing the format of the object.
    pooContentType        :: Maybe Text
  -- | Set what content encodings have been applied to the object and thus
  -- what decoding mechanisms must be applied to obtain the media-type
  -- referenced by the Content-Type header field.
  , pooContentEncoding    :: Maybe Text
  -- | Set presentational information for the object.
  , pooContentDisposition :: Maybe Text
  -- | Set to specify caching behavior for the object along the
  -- request/reply chain.
  , pooCacheControl       :: Maybe Text
  -- | Set to describe the language(s) intended for the audience.
  , pooContentLanguage    :: Maybe Text
  -- | Set to @STANDARD@ or @REDUCED_REDUNDANCY@ depending on your
  -- performance needs, storage class is @STANDARD@ by default (i.e
  -- when Nothing is passed).
  , pooStorageClass       :: Maybe Text
  -- | Set user defined metadata to store with the object.
  , pooUserMetadata       :: [(Text, Text)]
  -- | Set number of worker threads used to upload an object.
  , pooNumThreads         :: Maybe Word
  -- | Set object encryption parameters for the request.
  , pooSSE                :: Maybe SSE
  }

-- | Provide default `PutObjectOptions`.
defaultPutObjectOptions :: PutObjectOptions
defaultPutObjectOptions = PutObjectOptions Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing

-- | If the given header name has the @X-Amz-Meta-@ prefix, it is
-- stripped and a Just is returned.
userMetadataHeaderNameMaybe :: Text -> Maybe Text
userMetadataHeaderNameMaybe k =
    let prefix = T.toCaseFold "X-Amz-Meta-"
        n = T.length prefix
    in if T.toCaseFold (T.take n k) == prefix
       then Just (T.drop n k)
       else Nothing

addXAmzMetaPrefix :: Text -> Text
addXAmzMetaPrefix s | isJust (userMetadataHeaderNameMaybe s) = s
                    | otherwise = "X-Amz-Meta-" <> s

mkHeaderFromMetadata :: [(Text, Text)] -> [HT.Header]
mkHeaderFromMetadata = map (\(x, y) -> (mk $ encodeUtf8 $ addXAmzMetaPrefix $ x, encodeUtf8 y))

pooToHeaders :: PutObjectOptions -> [HT.Header]
pooToHeaders poo = userMetadata
                   ++ (catMaybes $ map tupToMaybe (zipWith (,) names values))
                   ++ maybe [] toPutObjectHeaders (pooSSE poo)
  where
    tupToMaybe (k, Just v)  = Just (k, v)
    tupToMaybe (_, Nothing) = Nothing

    userMetadata = mkHeaderFromMetadata $ pooUserMetadata poo

    names = ["content-type",
             "content-encoding",
             "content-disposition",
             "content-language",
             "cache-control",
             "x-amz-storage-class"]
    values = map (fmap encodeUtf8 . (poo &))
             [pooContentType, pooContentEncoding,
              pooContentDisposition, pooContentLanguage,
              pooCacheControl, pooStorageClass]


-- |
-- BucketInfo returned for list buckets call
data BucketInfo = BucketInfo {
    biName         :: Bucket
  , biCreationDate :: UTCTime
  } deriving (Show, Eq)

-- | A type alias to represent a part-number for multipart upload
type PartNumber = Int16

-- | A type alias to represent an upload-id for multipart upload
type UploadId = Text

-- | A type to represent a part-number and etag.
type PartTuple = (PartNumber, ETag)

-- | Represents result from a listing of object parts of an ongoing
-- multipart upload.
data ListPartsResult = ListPartsResult {
    lprHasMore  :: Bool
  , lprNextPart :: Maybe Int
  , lprParts    :: [ObjectPartInfo]
 } deriving (Show, Eq)

-- | Represents information about an object part in an ongoing
-- multipart upload.
data ObjectPartInfo = ObjectPartInfo {
    opiNumber  :: PartNumber
  , opiETag    :: ETag
  , opiSize    :: Int64
  , opiModTime :: UTCTime
  } deriving (Show, Eq)

-- | Represents result from a listing of incomplete uploads to a
-- bucket.
data ListUploadsResult = ListUploadsResult {
    lurHasMore    :: Bool
  , lurNextKey    :: Maybe Text
  , lurNextUpload :: Maybe Text
  , lurUploads    :: [(Object, UploadId, UTCTime)]
  , lurCPrefixes  :: [Text]
  } deriving (Show, Eq)

-- | Represents information about a multipart upload.
data UploadInfo = UploadInfo {
    uiKey      :: Object
  , uiUploadId :: UploadId
  , uiInitTime :: UTCTime
  , uiSize     :: Int64
  } deriving (Show, Eq)

-- | Represents result from a listing of objects in a bucket.
data ListObjectsResult = ListObjectsResult {
    lorHasMore   :: Bool
  , lorNextToken :: Maybe Text
  , lorObjects   :: [ObjectInfo]
  , lorCPrefixes :: [Text]
  } deriving (Show, Eq)

-- | Represents result from a listing of objects version 1 in a bucket.
data ListObjectsV1Result = ListObjectsV1Result {
    lorHasMore'   :: Bool
  , lorNextMarker :: Maybe Text
  , lorObjects'   :: [ObjectInfo]
  , lorCPrefixes' :: [Text]
  } deriving (Show, Eq)

-- | Represents information about an object.
data ObjectInfo = ObjectInfo
  { oiObject       :: Object -- ^ Object key
  , oiModTime      :: UTCTime -- ^ Modification time of the object
  , oiETag         :: ETag -- ^ ETag of the object
  , oiSize         :: Int64 -- ^ Size of the object in bytes
  , oiUserMetadata :: H.HashMap Text Text -- ^ A map of user-metadata
                                          -- pairs stored with an
                                          -- object (keys will not
                                          -- have the @X-Amz-Meta-@
                                          -- prefix).
  , oiMetadata     :: H.HashMap Text Text -- ^ A map of metadata
                                          -- key-value pairs (not
                                          -- including the
                                          -- user-metadata pairs)
  } deriving (Show, Eq)

-- | Represents source object in server-side copy object
data SourceInfo = SourceInfo
  { srcBucket            :: Text -- ^ Bucket containing the source object
  , srcObject            :: Text -- ^ Source object key
  , srcRange             :: Maybe (Int64, Int64) -- ^ Source object
                                                 -- byte-range
                                                 -- (inclusive)
  , srcIfMatch           :: Maybe Text -- ^ ETag condition on source -
                                       -- object is copied only if the
                                       -- source object's ETag matches
                                       -- this value.
  , srcIfNoneMatch       :: Maybe Text -- ^ ETag not match condition
                                       -- on source - object is copied
                                       -- if ETag does not match this
                                       -- value.
  , srcIfModifiedSince   :: Maybe UTCTime -- ^ Copy source object only
                                          -- if the source has been
                                          -- modified since this time.
  , srcIfUnmodifiedSince :: Maybe UTCTime -- ^ Copy source object only
                                          -- if the source has been
                                          -- un-modified since this
                                          -- given time.
  } deriving (Show, Eq)

-- | Provide a default for `SourceInfo`
defaultSourceInfo :: SourceInfo
defaultSourceInfo = SourceInfo "" "" Nothing Nothing Nothing Nothing Nothing

-- | Represents destination object in server-side copy object
data DestinationInfo = DestinationInfo
                       { dstBucket :: Text -- ^ Destination bucket
                       , dstObject :: Text -- ^ Destination object key
                       } deriving (Show, Eq)

-- | Provide a default for `DestinationInfo`
defaultDestinationInfo :: DestinationInfo
defaultDestinationInfo = DestinationInfo "" ""

-- | Data type for options when getting an object from the
-- service. Start with the empty `defaultGetObjectOptions` and modify
-- it using the goo* functions.
data GetObjectOptions = GetObjectOptions {
    -- | Set object's data of given offset begin and end,
    -- [ByteRangeFromTo 0 9] means first ten bytes of the source object.
    gooRange             :: Maybe ByteRange
    -- | Set matching ETag condition, GetObject which matches the following
    -- ETag.
  , gooIfMatch           :: Maybe ETag
    -- | Set matching ETag none condition, GetObject which does not match
    -- the following ETag.
  , gooIfNoneMatch       :: Maybe ETag
    -- | Set object unmodified condition, GetObject unmodified since given time.
  , gooIfUnmodifiedSince :: Maybe UTCTime
    -- | Set object modified condition, GetObject modified since given time.
  , gooIfModifiedSince   :: Maybe UTCTime
    -- | Specify SSE-C key
  , gooSSECKey           :: Maybe SSECKey
  }

-- | Provide default  `GetObjectOptions`.
defaultGetObjectOptions :: GetObjectOptions
defaultGetObjectOptions =
    GetObjectOptions Nothing Nothing Nothing Nothing Nothing Nothing

gooToHeaders :: GetObjectOptions -> [HT.Header]
gooToHeaders goo = rangeHdr ++ zip names values
                   ++ maybe [] (toPutObjectHeaders . SSEC) (gooSSECKey goo)
  where
    names = ["If-Match",
             "If-None-Match",
             "If-Unmodified-Since",
             "If-Modified-Since"]
    values = mapMaybe (fmap encodeUtf8 . (goo &))
             [gooIfMatch, gooIfNoneMatch,
              fmap formatRFC1123 . gooIfUnmodifiedSince,
              fmap formatRFC1123 . gooIfModifiedSince]
    rangeHdr = maybe [] (\a -> [(hRange, HT.renderByteRanges [a])])
               $ gooRange goo

-- | Data type returned by 'getObject' representing the object being
-- retrieved. Use the @gor*@ functions to access its contents.
data GetObjectResponse = GetObjectResponse {
      -- | ObjectInfo of the object being retrieved.
      gorObjectInfo   :: ObjectInfo
      -- | A conduit of the bytes of the object.
    , gorObjectStream :: C.ConduitM () ByteString Minio ()
    }

-- | A data-type for events that can occur in the object storage
-- server. Reference:
-- https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html#supported-notification-event-types
data Event = ObjectCreated
           | ObjectCreatedPut
           | ObjectCreatedPost
           | ObjectCreatedCopy
           | ObjectCreatedMultipartUpload
           | ObjectRemoved
           | ObjectRemovedDelete
           | ObjectRemovedDeleteMarkerCreated
           | ReducedRedundancyLostObject
           deriving (Eq)

instance Show Event where
  show ObjectCreated                    = "s3:ObjectCreated:*"
  show ObjectCreatedPut                 = "s3:ObjectCreated:Put"
  show ObjectCreatedPost                = "s3:ObjectCreated:Post"
  show ObjectCreatedCopy                = "s3:ObjectCreated:Copy"
  show ObjectCreatedMultipartUpload     = "s3:ObjectCreated:MultipartUpload"
  show ObjectRemoved                    = "s3:ObjectRemoved:*"
  show ObjectRemovedDelete              = "s3:ObjectRemoved:Delete"
  show ObjectRemovedDeleteMarkerCreated = "s3:ObjectRemoved:DeleteMarkerCreated"
  show ReducedRedundancyLostObject      = "s3:ReducedRedundancyLostObject"

textToEvent :: Text -> Maybe Event
textToEvent t = case t of
  "s3:ObjectCreated:*"                   -> Just ObjectCreated
  "s3:ObjectCreated:Put"                 -> Just ObjectCreatedPut
  "s3:ObjectCreated:Post"                -> Just ObjectCreatedPost
  "s3:ObjectCreated:Copy"                -> Just ObjectCreatedCopy
  "s3:ObjectCreated:MultipartUpload"     -> Just ObjectCreatedMultipartUpload
  "s3:ObjectRemoved:*"                   -> Just ObjectRemoved
  "s3:ObjectRemoved:Delete"              -> Just ObjectRemovedDelete
  "s3:ObjectRemoved:DeleteMarkerCreated" -> Just ObjectRemovedDeleteMarkerCreated
  "s3:ReducedRedundancyLostObject"       -> Just ReducedRedundancyLostObject
  _                                      -> Nothing


-- | Filter data type - part of notification configuration
data Filter = Filter
  { fFilter :: FilterKey
  } deriving (Show, Eq)

-- | defaultFilter is empty, used to create a notification
-- configuration.
defaultFilter :: Filter
defaultFilter = Filter defaultFilterKey

-- | FilterKey contains FilterRules, and is part of a Filter.
data FilterKey = FilterKey
  { fkKey :: FilterRules
  } deriving (Show, Eq)

-- | defaultFilterKey is empty, used to create notification
-- configuration.
defaultFilterKey :: FilterKey
defaultFilterKey = FilterKey defaultFilterRules

-- | FilterRules represents a collection of `FilterRule`s.
data FilterRules = FilterRules
  { frFilterRules :: [FilterRule]
  } deriving (Show, Eq)

-- | defaultFilterRules is empty, used to create notification
-- configuration.
defaultFilterRules :: FilterRules
defaultFilterRules = FilterRules []


-- | A filter rule that can act based on the suffix or prefix of an
-- object. As an example, let's create two filter rules:
--
--    > let suffixRule = FilterRule "suffix" ".jpg"
--    > let prefixRule = FilterRule "prefix" "images/"
--
-- The @suffixRule@ restricts the notification to be triggered only
-- for objects having a suffix of ".jpg", and the @prefixRule@
-- restricts it to objects having a prefix of "images/".
data FilterRule = FilterRule
  { frName  :: Text
  , frValue :: Text
  } deriving (Show, Eq)

-- | Arn is an alias of Text
type Arn = Text

-- | A data-type representing the configuration for a particular
-- notification system. It could represent a Queue, Topic or Lambda
-- Function configuration.
data NotificationConfig = NotificationConfig
  { ncId     :: Text
  , ncArn    :: Arn
  , ncEvents :: [Event]
  , ncFilter :: Filter
  } deriving (Show, Eq)

-- | A data-type to represent bucket notification configuration. It is
-- a collection of queue, topic or lambda function configurations. The
-- structure of the types follow closely the XML representation
-- described at
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTnotification.html>
data Notification = Notification
  { nQueueConfigurations         :: [NotificationConfig]
  , nTopicConfigurations         :: [NotificationConfig]
  , nCloudFunctionConfigurations :: [NotificationConfig]
  } deriving (Eq, Show)

-- | The default notification configuration is empty.
defaultNotification :: Notification
defaultNotification = Notification [] [] []


--------------------------------------------------------------------------
-- Select API Related Types
--------------------------------------------------------------------------

-- | SelectRequest represents the Select API call. Use the
-- `selectRequest` function to create a value of this type.
data SelectRequest = SelectRequest
  { srExpression             :: Text
  , srExpressionType         :: ExpressionType
  , srInputSerialization     :: InputSerialization
  , srOutputSerialization    :: OutputSerialization
  , srRequestProgressEnabled :: Maybe Bool
  } deriving (Eq, Show)

data ExpressionType = SQL
                    deriving (Eq, Show)

-- | InputSerialization represents format information of the input
-- object being queried. Use one of the smart constructors such as
-- `defaultCsvInput` as a starting value, and add compression info
-- using `setInputCompressionType`
data InputSerialization = InputSerialization
  { isCompressionType :: Maybe CompressionType
  , isFormatInfo      :: InputFormatInfo
  } deriving (Eq, Show)

-- | Data type representing the compression setting in a Select
-- request.
data CompressionType = CompressionTypeNone
                     | CompressionTypeGzip
                     | CompressionTypeBzip2
                     deriving (Eq, Show)

-- | Data type representing input object format information in a
-- Select request.
data InputFormatInfo = InputFormatCSV CSVInputProp
                     | InputFormatJSON JSONInputProp
                     | InputFormatParquet
                     deriving (Eq, Show)

-- | defaultCsvInput returns InputSerialization with default CSV
-- format, and without any compression setting.
defaultCsvInput :: InputSerialization
defaultCsvInput = InputSerialization Nothing (InputFormatCSV defaultCSVProp)

-- | linesJsonInput returns InputSerialization with JSON line based
-- format with no compression setting.
linesJsonInput :: InputSerialization
linesJsonInput = InputSerialization Nothing
                 (InputFormatJSON $ JSONInputProp JSONTypeLines)

-- | documentJsonInput returns InputSerialization with JSON document
-- based format with no compression setting.
documentJsonInput :: InputSerialization
documentJsonInput = InputSerialization Nothing
                    (InputFormatJSON $ JSONInputProp JSONTypeDocument)

-- | defaultParquetInput returns InputSerialization with Parquet
-- format, and no compression setting.
defaultParquetInput :: InputSerialization
defaultParquetInput = InputSerialization Nothing InputFormatParquet

-- | setInputCompressionType sets the compression type for the input
-- of the SelectRequest
setInputCompressionType :: CompressionType -> SelectRequest
                        -> SelectRequest
setInputCompressionType c i =
    let is = srInputSerialization i
        is' = is { isCompressionType = Just c }
    in i { srInputSerialization = is' }

-- | defaultCsvOutput returns OutputSerialization with default CSV
-- format.
defaultCsvOutput :: OutputSerialization
defaultCsvOutput = OutputSerializationCSV defaultCSVProp

-- | defaultJsonInput returns OutputSerialization with default JSON
-- format.
defaultJsonOutput :: OutputSerialization
defaultJsonOutput = OutputSerializationJSON (JSONOutputProp Nothing)

-- | selectRequest is used to build a `SelectRequest`
-- value. @selectRequest query inputSer outputSer@ represents a
-- SelectRequest with the SQL query text given by @query@, the input
-- serialization settings (compression format and format information)
-- @inputSer@ and the output serialization settings @outputSer@.
selectRequest :: Text -> InputSerialization -> OutputSerialization
              -> SelectRequest
selectRequest sqlQuery inputSer outputSer =
    SelectRequest { srExpression = sqlQuery
                  , srExpressionType = SQL
                  , srInputSerialization = inputSer
                  , srOutputSerialization = outputSer
                  , srRequestProgressEnabled = Nothing
                  }

-- | setRequestProgressEnabled sets the flag for turning on progress
-- messages when the Select response is being streamed back to the
-- client.
setRequestProgressEnabled :: Bool -> SelectRequest -> SelectRequest
setRequestProgressEnabled enabled sr =
    sr { srRequestProgressEnabled = Just enabled }

type CSVInputProp = CSVProp

-- | CSVProp represents CSV format properties. It is built up using
-- the Monoid instance.
data CSVProp = CSVProp (H.HashMap Text Text)
             deriving (Eq, Show)

#if (__GLASGOW_HASKELL__ >= 804)
instance Semigroup CSVProp where
    (CSVProp a) <> (CSVProp b) = CSVProp (b <> a)
#endif

instance Monoid CSVProp where
    mempty = CSVProp mempty
#if (__GLASGOW_HASKELL__ < 804)
    mappend (CSVProp a) (CSVProp b) = CSVProp (b <> a)
#endif

defaultCSVProp :: CSVProp
defaultCSVProp = mempty

-- | Specify the CSV record delimiter property.
recordDelimiter :: Text -> CSVProp
recordDelimiter = CSVProp . H.singleton "RecordDelimiter"

-- | Specify the CSV field delimiter property.
fieldDelimiter :: Text -> CSVProp
fieldDelimiter = CSVProp . H.singleton "FieldDelimiter"

-- | Specify the CSV quote character property.
quoteCharacter :: Text -> CSVProp
quoteCharacter = CSVProp . H.singleton "QuoteCharacter"

-- | Specify the CSV quote-escape character property.
quoteEscapeCharacter :: Text -> CSVProp
quoteEscapeCharacter = CSVProp . H.singleton "QuoteEscapeCharacter"

-- | FileHeaderInfo specifies information about column headers for CSV
-- format.
data FileHeaderInfo
    = FileHeaderNone -- ^ No column headers are present
    | FileHeaderUse -- ^ Headers are present and they should be used
    | FileHeaderIgnore -- ^ Header are present, but should be ignored
    deriving (Eq, Show)

-- | Specify the CSV file header info property.
fileHeaderInfo :: FileHeaderInfo -> CSVProp
fileHeaderInfo = CSVProp . H.singleton "FileHeaderInfo" . toString
  where
    toString FileHeaderNone   = "NONE"
    toString FileHeaderUse    = "USE"
    toString FileHeaderIgnore = "IGNORE"

-- | Specify the CSV comment character property. Lines starting with
-- this character are ignored by the server.
commentCharacter :: Text -> CSVProp
commentCharacter = CSVProp . H.singleton "Comments"

-- | Allow quoted record delimiters inside a row using this property.
allowQuotedRecordDelimiter :: CSVProp
allowQuotedRecordDelimiter = CSVProp $ H.singleton "AllowQuotedRecordDelimiter" "TRUE"

-- | Set the CSV format properties in the InputSerialization.
setInputCSVProps :: CSVProp -> InputSerialization -> InputSerialization
setInputCSVProps p is = is { isFormatInfo = InputFormatCSV p }

-- | Set the CSV format properties in the OutputSerialization.
outputCSVFromProps :: CSVProp -> OutputSerialization
outputCSVFromProps p = OutputSerializationCSV p

data JSONInputProp = JSONInputProp { jsonipType :: JSONType }
                   deriving (Eq, Show)

data JSONType = JSONTypeDocument | JSONTypeLines
              deriving (Eq, Show)


-- | OutputSerialization represents output serialization settings for
-- the SelectRequest. Use `defaultCsvOutput` or `defaultJsonOutput` as
-- a starting point.
data OutputSerialization = OutputSerializationJSON JSONOutputProp
                         | OutputSerializationCSV CSVOutputProp
                         deriving (Eq, Show)

type CSVOutputProp = CSVProp

-- | quoteFields is an output serialization parameter
quoteFields :: QuoteFields -> CSVProp
quoteFields q = CSVProp $ H.singleton "QuoteFields" $
  case q of
    QuoteFieldsAsNeeded -> "ASNEEDED"
    QuoteFieldsAlways   -> "ALWAYS"

-- | Represent the QuoteField setting.
data QuoteFields = QuoteFieldsAsNeeded | QuoteFieldsAlways
                 deriving (Eq, Show)

data JSONOutputProp = JSONOutputProp { jsonopRecordDelimiter :: Maybe Text }
                    deriving (Eq, Show)

-- | Set the output record delimiter for JSON format
outputJSONFromRecordDelimiter :: Text -> OutputSerialization
outputJSONFromRecordDelimiter t =
    OutputSerializationJSON (JSONOutputProp $ Just t)

-- Response related types

-- | An EventMessage represents each kind of message received from the server.
data EventMessage = ProgressEventMessage { emProgress :: Progress }
                  | StatsEventMessage { emStats :: Stats }
                  | RequestLevelErrorMessage { emErrorCode    :: Text
                                             , emErrorMessage :: Text
                                             }
                  | RecordPayloadEventMessage { emPayloadBytes :: ByteString }
                  deriving (Eq, Show)

data MsgHeaderName = MessageType
                   | EventType
                   | ContentType
                   | ErrorCode
                   | ErrorMessage
                   deriving (Eq, Show)

msgHeaderValueType :: Word8
msgHeaderValueType = 7

type MessageHeader = (MsgHeaderName, Text)

-- | Represent the progress event returned in the Select response.
data Progress = Progress { pBytesScanned   :: Int64
                         , pBytesProcessed :: Int64
                         , pBytesReturned  :: Int64
                         }
              deriving (Eq, Show)

-- | Represent the stats event returned at the end of the Select
-- response.
type Stats = Progress

--------------------------------------------------------------------------
-- Select API Related Types End
--------------------------------------------------------------------------

-- | Represents different kinds of payload that are used with S3 API
-- requests.
data Payload
    = PayloadBS ByteString
    | PayloadH Handle Int64 Int64 -- file handle, offset and length
    | PayloadC Int64 (C.ConduitT () ByteString (ResourceT IO) ()) -- length and byte source

defaultPayload :: Payload
defaultPayload = PayloadBS ""

data AdminReqInfo = AdminReqInfo {
    ariMethod      :: Method
  , ariPayloadHash :: Maybe ByteString
  , ariPayload     :: Payload
  , ariPath        :: ByteString
  , ariHeaders     :: [Header]
  , ariQueryParams :: Query
  }

data S3ReqInfo = S3ReqInfo
  { riMethod        :: Method
  , riBucket        :: Maybe Bucket
  , riObject        :: Maybe Object
  , riQueryParams   :: Query
  , riHeaders       :: [Header]
  , riPayload       :: Payload
  , riPayloadHash   :: Maybe ByteString
  , riRegion        :: Maybe Region
  , riNeedsLocation :: Bool
  }

defaultS3ReqInfo :: S3ReqInfo
defaultS3ReqInfo = S3ReqInfo HT.methodGet Nothing Nothing
                   [] [] defaultPayload Nothing Nothing True

getS3Path :: Maybe Bucket -> Maybe Object -> ByteString
getS3Path b o =
  let segments = map toS $ catMaybes $ b : bool [] [o] (isJust b)
  in
    B.concat ["/", B.intercalate "/" segments]

-- | Time to expire for a presigned URL. It interpreted as a number of
-- seconds. The maximum duration that can be specified is 7 days.
type UrlExpiry = Int

type RegionMap = H.HashMap Bucket Region

-- | The Minio Monad - all computations accessing object storage
-- happens in it.
newtype Minio a = Minio {
  unMinio :: ReaderT MinioConn (ResourceT IO) a
  }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader MinioConn
    , MonadResource
    )

instance MonadUnliftIO Minio where
  askUnliftIO = Minio $ ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r . unMinio))

-- | MinioConn holds connection info and a connection pool to allow
-- for efficient resource re-use.
data MinioConn = MinioConn
  { mcConnInfo    :: ConnectInfo
  , mcConnManager :: NC.Manager
  , mcRegionMap   :: MVar RegionMap
  }

class HasSvcNamespace env where
  getSvcNamespace :: env -> Text

instance HasSvcNamespace MinioConn where
  getSvcNamespace env = let host = connectHost $ mcConnInfo env
                            in if | host  == "storage.googleapis.com" ->
                                    "http://doc.s3.amazonaws.com/2006-03-01"
                                  | otherwise ->
                                    "http://s3.amazonaws.com/doc/2006-03-01/"

-- | Takes connection information and returns a connection object to
-- be passed to 'runMinio'. The returned value can be kept in the
-- application environment and passed to `runMinioWith` whenever
-- object storage is accessed.
connect :: ConnectInfo -> IO MinioConn
connect ci = do
  let settings | connectIsSecure ci && connectDisableTLSCertValidation ci =
                 let badTlsSettings = Conn.TLSSettingsSimple True False False
                 in TLS.mkManagerSettings badTlsSettings Nothing
               | connectIsSecure ci = NC.tlsManagerSettings
               | otherwise = defaultManagerSettings
  mgr <- NC.newManager settings
  mkMinioConn ci mgr

-- | Run the computation accessing object storage using the given
-- `MinioConn`. This reuses connections, but otherwise it is similar
-- to `runMinio`.
runMinioWith :: MinioConn -> Minio a -> IO (Either MinioErr a)
runMinioWith conn m = runResourceT $ runMinioResWith conn m

-- | Given `ConnectInfo` and a HTTP connection manager, create a
-- `MinioConn`.
mkMinioConn :: ConnectInfo -> NC.Manager -> IO MinioConn
mkMinioConn ci mgr = do
    rMapMVar <- M.newMVar H.empty
    return $ MinioConn ci mgr rMapMVar

-- | Run the Minio action and return the result or an error.
runMinio :: ConnectInfo -> Minio a -> IO (Either MinioErr a)
runMinio ci m = do
  conn <- connect ci
  runResourceT $ runMinioResWith conn m

-- | Similar to 'runMinioWith'. Allows applications to allocate/release
-- its resources along side MinIO's internal resources.
runMinioResWith :: MinioConn -> Minio a -> ResourceT IO (Either MinioErr a)
runMinioResWith conn m =
  flip runReaderT conn . unMinio $
    fmap Right m `U.catches`
    [ U.Handler handlerServiceErr
    , U.Handler handlerHE
    , U.Handler handlerFE
    , U.Handler handlerValidation
    ]
  where
    handlerServiceErr = return . Left . MErrService
    handlerHE = return . Left . MErrHTTP
    handlerFE = return . Left . MErrIO
    handlerValidation = return . Left . MErrValidation

-- | Similar to 'runMinio'. Allows applications to allocate/release
-- its resources along side MinIO's internal resources.
runMinioRes :: ConnectInfo -> Minio a -> ResourceT IO (Either MinioErr a)
runMinioRes ci m = do
  conn <- liftIO $ connect ci
  runMinioResWith conn m

s3Name :: Text -> Text -> Name
s3Name ns s = Name s (Just ns) Nothing

-- | Format as per RFC 1123.
formatRFC1123 :: UTCTime -> T.Text
formatRFC1123 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X %Z"
