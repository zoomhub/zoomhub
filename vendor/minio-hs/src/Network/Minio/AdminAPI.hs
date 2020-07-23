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

module Network.Minio.AdminAPI
  ( -- * MinIO Admin API
    --------------------
    -- | Provides MinIO admin API and related types. It is in
    -- experimental state.
    DriveInfo(..)
  , ErasureInfo(..)
  , Backend(..)
  , ConnStats(..)
  , HttpStats(..)
  , ServerProps(..)
  , CountNAvgTime(..)
  , StorageClass(..)
  , StorageInfo(..)
  , SIData(..)
  , ServerInfo(..)
  , getServerInfo

  , HealOpts(..)
  , HealResultItem(..)
  , HealStatus(..)
  , HealStartResp(..)
  , startHeal
  , forceStartHeal
  , getHealStatus

  , SetConfigResult(..)
  , NodeSummary(..)
  , setConfig
  , getConfig

  , ServerVersion(..)
  , ServiceStatus(..)
  , serviceStatus

  , ServiceAction(..)
  , serviceSendAction
  ) where

import           Data.Aeson                (FromJSON, ToJSON, Value (Object),
                                            eitherDecode, object, pairs,
                                            parseJSON, toEncoding, toJSON,
                                            withObject, withText, (.:), (.:?),
                                            (.=))
import qualified Data.Aeson                as A
import           Data.Aeson.Types          (typeMismatch)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import           Data.Time                 (NominalDiffTime, getCurrentTime)
import           Network.HTTP.Conduit      (Response)
import qualified Network.HTTP.Conduit      as NC
import qualified Network.HTTP.Types        as HT
import           Network.HTTP.Types.Header (hHost)

import           Lib.Prelude

import           Network.Minio.APICommon
import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.Sign.V4
import           Network.Minio.Utils

data DriveInfo = DriveInfo
                 { diUuid     :: Text
                 , diEndpoint :: Text
                 , diState    :: Text
                 } deriving (Eq, Show)

instance FromJSON DriveInfo where
    parseJSON = withObject "DriveInfo" $ \v -> DriveInfo
        <$> v .: "uuid"
        <*> v .: "endpoint"
        <*> v .: "state"

data StorageClass = StorageClass
                    { scParity :: Int
                    , scData   :: Int
                    } deriving (Eq, Show)

data ErasureInfo = ErasureInfo
                   { eiOnlineDisks       :: Int
                   , eiOfflineDisks      :: Int
                   , eiStandard          :: StorageClass
                   , eiReducedRedundancy :: StorageClass
                   , eiSets              :: [[DriveInfo]]
                   } deriving (Eq, Show)

instance FromJSON ErasureInfo where
    parseJSON = withObject "ErasureInfo" $ \v -> do
        onlineDisks <- v .: "OnlineDisks"
        offlineDisks <- v .: "OfflineDisks"
        stdClass <- StorageClass
                    <$> v .: "StandardSCData"
                    <*> v .: "StandardSCParity"
        rrClass <- StorageClass
                   <$>  v .: "RRSCData"
                   <*>  v .: "RRSCParity"
        sets <- v .: "Sets"
        return $ ErasureInfo onlineDisks offlineDisks stdClass rrClass sets

data Backend = BackendFS
             | BackendErasure ErasureInfo
             deriving (Eq, Show)

instance FromJSON Backend where
    parseJSON = withObject "Backend" $ \v -> do
        typ <- v .: "Type"
        case typ :: Int of
            1 -> return BackendFS
            2 -> BackendErasure <$> parseJSON (Object v)
            _ -> typeMismatch "BackendType" (Object v)

data ConnStats = ConnStats
    { csTransferred :: Int64
    , csReceived    :: Int64
    } deriving (Eq, Show)

instance FromJSON ConnStats where
    parseJSON = withObject "ConnStats" $ \v -> ConnStats
        <$> v .: "transferred"
        <*> v .: "received"

data ServerProps = ServerProps
    { spUptime   :: NominalDiffTime
    , spVersion  :: Text
    , spCommitId :: Text
    , spRegion   :: Text
    , spSqsArns  :: [Text]
    } deriving (Eq, Show)

instance FromJSON ServerProps where
    parseJSON = withObject "SIServer" $ \v -> do
        uptimeNs <- v .: "uptime"
        let uptime = uptimeNs / 1e9
        ver <- v .: "version"
        commitId <- v .: "commitID"
        region <- v .: "region"
        arn <- v .: "sqsARN"
        return $ ServerProps uptime ver commitId region arn

data StorageInfo = StorageInfo
    { siUsed    :: Int64
    , siBackend :: Backend
    } deriving (Eq, Show)

instance FromJSON StorageInfo where
    parseJSON = withObject "StorageInfo" $ \v -> StorageInfo
        <$> v .: "Used"
        <*> v .: "Backend"

data CountNAvgTime = CountNAvgTime
    {  caCount       :: Int64
    ,  caAvgDuration :: Text
    } deriving (Eq, Show)

instance FromJSON CountNAvgTime where
    parseJSON = withObject "CountNAvgTime" $ \v -> CountNAvgTime
        <$> v .: "count"
        <*> v .: "avgDuration"

data HttpStats = HttpStats
    { hsTotalHeads     :: CountNAvgTime
    , hsSuccessHeads   :: CountNAvgTime
    , hsTotalGets      :: CountNAvgTime
    , hsSuccessGets    :: CountNAvgTime
    , hsTotalPuts      :: CountNAvgTime
    , hsSuccessPuts    :: CountNAvgTime
    , hsTotalPosts     :: CountNAvgTime
    , hsSuccessPosts   :: CountNAvgTime
    , hsTotalDeletes   :: CountNAvgTime
    , hsSuccessDeletes :: CountNAvgTime
    } deriving (Eq, Show)

instance FromJSON HttpStats where
    parseJSON  = withObject "HttpStats" $ \v -> HttpStats
        <$> v .: "totalHEADs"
        <*> v .: "successHEADs"
        <*> v .: "totalGETs"
        <*> v .: "successGETs"
        <*> v .: "totalPUTs"
        <*> v .: "successPUTs"
        <*> v .: "totalPOSTs"
        <*> v .: "successPOSTs"
        <*> v .: "totalDELETEs"
        <*> v .: "successDELETEs"

data SIData = SIData
    { sdStorage   :: StorageInfo
    , sdConnStats :: ConnStats
    , sdHttpStats :: HttpStats
    , sdProps     :: ServerProps
    } deriving (Eq, Show)

instance FromJSON SIData where
    parseJSON = withObject "SIData" $ \v -> SIData
        <$> v .: "storage"
        <*> v .: "network"
        <*> v .: "http"
        <*> v .: "server"

data ServerInfo = ServerInfo
    { siError :: Text
    , siAddr  :: Text
    , siData  :: SIData
    } deriving (Eq, Show)

instance FromJSON ServerInfo where
    parseJSON = withObject "ServerInfo" $ \v -> ServerInfo
        <$> v .: "error"
        <*> v .: "addr"
        <*> v .: "data"

data ServerVersion = ServerVersion
  { svVersion  :: Text
  , svCommitId :: Text
  } deriving (Eq, Show)

instance FromJSON ServerVersion where
    parseJSON = withObject "ServerVersion" $ \v -> ServerVersion
      <$> v .: "version"
      <*> v .: "commitID"

data ServiceStatus = ServiceStatus
  { ssVersion :: ServerVersion
  , ssUptime  :: NominalDiffTime
  } deriving (Eq, Show)

instance FromJSON ServiceStatus where
    parseJSON = withObject "ServiceStatus" $ \v -> do
      serverVersion <- v .: "serverVersion"
      uptimeNs <- v .: "uptime"
      let uptime = uptimeNs / 1e9
      return $ ServiceStatus serverVersion uptime

data ServiceAction = ServiceActionRestart
                   | ServiceActionStop
                   deriving (Eq, Show)

instance ToJSON ServiceAction where
    toJSON a = object [ "action" .= serviceActionToText a ]

serviceActionToText :: ServiceAction -> Text
serviceActionToText a = case a of
  ServiceActionRestart -> "restart"
  ServiceActionStop    -> "stop"

adminPath :: ByteString
adminPath = "/minio/admin"

data HealStartResp = HealStartResp
  { hsrClientToken :: Text
  , hsrClientAddr  :: Text
  , hsrStartTime   :: UTCTime
  } deriving (Eq, Show)

instance FromJSON HealStartResp where
    parseJSON = withObject "HealStartResp" $ \v -> HealStartResp
        <$> v .: "clientToken"
        <*> v .: "clientAddress"
        <*> v .: "startTime"

data HealOpts = HealOpts
  { hoRecursive :: Bool
  , hoDryRun    :: Bool
  } deriving (Eq, Show)

instance ToJSON HealOpts where
  toJSON (HealOpts r d) =
    object ["recursive" .= r, "dryRun" .= d]
  toEncoding (HealOpts r d) =
    pairs ("recursive" .= r <> "dryRun" .= d)

instance FromJSON HealOpts where
    parseJSON = withObject "HealOpts" $ \v -> HealOpts
      <$> v .: "recursive"
      <*> v .: "dryRun"

data HealItemType = HealItemMetadata
                  | HealItemBucket
                  | HealItemBucketMetadata
                  | HealItemObject
                  deriving (Eq, Show)

instance FromJSON HealItemType where
    parseJSON = withText "HealItemType" $ \v -> case v of
      "metadata"        -> return HealItemMetadata
      "bucket"          -> return HealItemBucket
      "object"          -> return HealItemObject
      "bucket-metadata" -> return HealItemBucketMetadata
      _                 -> typeMismatch "HealItemType" (A.String v)

data NodeSummary = NodeSummary
  { nsName       :: Text
  , nsErrSet     :: Bool
  , nsErrMessage :: Text
  } deriving (Eq, Show)

instance FromJSON NodeSummary where
  parseJSON = withObject "NodeSummary" $ \v -> NodeSummary
    <$> v .: "name"
    <*> v .: "errSet"
    <*> v .: "errMsg"

data SetConfigResult = SetConfigResult
  { scrStatus      :: Bool
  , scrNodeSummary :: [NodeSummary]
  } deriving (Eq, Show)

instance FromJSON SetConfigResult where
  parseJSON = withObject "SetConfigResult" $ \v -> SetConfigResult
    <$> v .: "status"
    <*> v .: "nodeResults"

data HealResultItem = HealResultItem
  { hriResultIdx    :: Int
  , hriType         :: HealItemType
  , hriBucket       :: Bucket
  , hriObject       :: Object
  , hriDetail       :: Text
  , hriParityBlocks :: Maybe Int
  , hriDataBlocks   :: Maybe Int
  , hriDiskCount    :: Int
  , hriSetCount     :: Int
  , hriObjectSize   :: Int
  , hriBefore       :: [DriveInfo]
  , hriAfter        :: [DriveInfo]
  } deriving (Eq, Show)

instance FromJSON HealResultItem where
  parseJSON = withObject "HealResultItem" $ \v -> HealResultItem
    <$> v .: "resultId"
    <*> v .: "type"
    <*> v .: "bucket"
    <*> v .: "object"
    <*> v .: "detail"
    <*> v .:? "parityBlocks"
    <*> v .:? "dataBlocks"
    <*> v .: "diskCount"
    <*> v .: "setCount"
    <*> v .: "objectSize"
    <*> (do before <- v .: "before"
            before .: "drives")
    <*> (do after <- v .: "after"
            after .: "drives")

data HealStatus = HealStatus
  { hsSummary       :: Text
  , hsStartTime     :: UTCTime
  , hsSettings      :: HealOpts
  , hsNumDisks      :: Int
  , hsFailureDetail :: Maybe Text
  , hsItems         :: Maybe [HealResultItem]
  } deriving (Eq, Show)

instance FromJSON HealStatus where
  parseJSON = withObject "HealStatus" $ \v -> HealStatus
    <$> v .: "Summary"
    <*> v .: "StartTime"
    <*> v .: "Settings"
    <*> v .: "NumDisks"
    <*> v .:? "Detail"
    <*> v .: "Items"

healPath :: Maybe Bucket -> Maybe Text -> ByteString
healPath bucket prefix = do
  if (isJust bucket)
    then encodeUtf8 $ "v1/heal/" <> fromMaybe "" bucket <> "/"
         <> fromMaybe "" prefix
    else encodeUtf8 $ "v1/heal/"

-- | Get server version and uptime.
serviceStatus :: Minio ServiceStatus
serviceStatus = do
    rsp <- executeAdminRequest AdminReqInfo { ariMethod = HT.methodGet
                                            , ariPayload = PayloadBS B.empty
                                            , ariPayloadHash = Nothing
                                            , ariPath = "v1/service"
                                            , ariHeaders = []
                                            , ariQueryParams = []
                                            }

    let rspBS = NC.responseBody rsp
    case eitherDecode rspBS of
        Right ss -> return ss
        Left err -> throwIO $ MErrVJsonParse $ T.pack err

-- | Send service restart or stop action to MinIO server.
serviceSendAction :: ServiceAction -> Minio ()
serviceSendAction action = do
    let payload = PayloadBS $ LBS.toStrict $ A.encode action
    void $ executeAdminRequest AdminReqInfo { ariMethod = HT.methodPost
                                            , ariPayload = payload
                                            , ariPayloadHash = Nothing
                                            , ariPath = "v1/service"
                                            , ariHeaders = []
                                            , ariQueryParams = []
                                            }

-- | Get the current config file from server.
getConfig :: Minio ByteString
getConfig = do
    rsp <- executeAdminRequest AdminReqInfo { ariMethod = HT.methodGet
                                            , ariPayload = PayloadBS B.empty
                                            , ariPayloadHash = Nothing
                                            , ariPath = "v1/config"
                                            , ariHeaders = []
                                            , ariQueryParams = []
                                            }
    return $ LBS.toStrict $ NC.responseBody rsp

-- | Set a new config to the server.
setConfig :: ByteString -> Minio SetConfigResult
setConfig config = do
    rsp <- executeAdminRequest AdminReqInfo { ariMethod = HT.methodPut
                                            , ariPayload = PayloadBS config
                                            , ariPayloadHash = Nothing
                                            , ariPath = "v1/config"
                                            , ariHeaders = []
                                            , ariQueryParams = []
                                            }

    let rspBS = NC.responseBody rsp
    case eitherDecode rspBS of
        Right scr -> return scr
        Left err  -> throwIO $ MErrVJsonParse $ T.pack err

-- | Get the progress of currently running heal task, this API should be
-- invoked right after `startHeal`. `token` is obtained after `startHeal`
-- which should be used to get the heal status.
getHealStatus :: Maybe Bucket -> Maybe Text -> Text -> Minio HealStatus
getHealStatus bucket prefix token = do
    when (isNothing bucket && isJust prefix) $ throwIO MErrVInvalidHealPath
    let qparams = HT.queryTextToQuery [("clientToken", Just token)]
    rsp <- executeAdminRequest AdminReqInfo { ariMethod = HT.methodPost
                                            , ariPayload = PayloadBS B.empty
                                            , ariPayloadHash = Nothing
                                            , ariPath = healPath bucket prefix
                                            , ariHeaders = []
                                            , ariQueryParams = qparams
                                            }
    let rspBS = NC.responseBody rsp
    case eitherDecode rspBS of
        Right hs -> return hs
        Left err -> throwIO $ MErrVJsonParse $ T.pack err

doHeal :: Maybe Bucket -> Maybe Text -> HealOpts -> Bool -> Minio HealStartResp
doHeal bucket prefix opts forceStart = do
    when (isNothing bucket && isJust prefix) $ throwIO MErrVInvalidHealPath
    let payload = PayloadBS $ LBS.toStrict $ A.encode opts
    let qparams = bool [] (HT.queryTextToQuery [("forceStart", Just "true")])
                  forceStart

    rsp <- executeAdminRequest AdminReqInfo { ariMethod = HT.methodPost
                                            , ariPayload = payload
                                            , ariPayloadHash = Nothing
                                            , ariPath = healPath bucket prefix
                                            , ariHeaders = []
                                            , ariQueryParams = qparams
                                            }

    let rspBS = NC.responseBody rsp
    case eitherDecode rspBS of
        Right hsr -> return hsr
        Left err  -> throwIO $ MErrVJsonParse $ T.pack err

-- | Start a heal sequence that scans data under given (possible empty)
-- `bucket` and `prefix`. The `recursive` bool turns on recursive
-- traversal under the given path. `dryRun` does not mutate on-disk data,
-- but performs data validation. Two heal sequences on overlapping paths
-- may not be initiated. The progress of a heal should be followed using
-- the `HealStatus` API. The server accumulates results of the heal
-- traversal and waits for the client to receive and acknowledge
-- them using the status API
startHeal :: Maybe Bucket -> Maybe Text -> HealOpts -> Minio HealStartResp
startHeal bucket prefix opts = doHeal bucket prefix opts False

-- | Similar to start a heal sequence, but force start a new heal sequence
-- even if an active heal is under progress.
forceStartHeal :: Maybe Bucket -> Maybe Text -> HealOpts -> Minio HealStartResp
forceStartHeal bucket prefix opts = doHeal bucket prefix opts True

-- | Fetches information for all cluster nodes, such as server
-- properties, storage information, network statistics, etc.
getServerInfo :: Minio [ServerInfo]
getServerInfo = do
    rsp <- executeAdminRequest AdminReqInfo { ariMethod = HT.methodGet
                                            , ariPayload = PayloadBS B.empty
                                            , ariPayloadHash = Nothing
                                            , ariPath = "v1/info"
                                            , ariHeaders = []
                                            , ariQueryParams = []
                                            }
    let rspBS = NC.responseBody rsp
    case eitherDecode rspBS of
        Right si -> return si
        Left err -> throwIO $ MErrVJsonParse $ T.pack err

executeAdminRequest :: AdminReqInfo -> Minio (Response LByteString)
executeAdminRequest ari = do
    req <- buildAdminRequest ari
    mgr <- asks mcConnManager
    httpLbs req mgr

buildAdminRequest :: AdminReqInfo -> Minio NC.Request
buildAdminRequest areq = do
    ci <- asks mcConnInfo
    sha256Hash <- if | connectIsSecure ci ->
                       -- if secure connection
                       return "UNSIGNED-PAYLOAD"

                       -- otherwise compute sha256
                     | otherwise -> getPayloadSHA256Hash (ariPayload areq)

    timeStamp <- liftIO getCurrentTime

    let hostHeader = (hHost, getHostAddr ci)
        newAreq = areq { ariPayloadHash = Just sha256Hash
                       , ariHeaders = hostHeader
                                    : sha256Header sha256Hash
                                    : ariHeaders areq
                       }
        signReq = toRequest ci newAreq
        sp = SignParams (connectAccessKey ci) (connectSecretKey ci)
             timeStamp Nothing Nothing (ariPayloadHash newAreq)
        signHeaders = signV4 sp signReq

    -- Update signReq with Authorization header containing v4 signature
    return signReq {
        NC.requestHeaders = ariHeaders newAreq ++ mkHeaderFromPairs signHeaders
        }
  where
    toRequest :: ConnectInfo -> AdminReqInfo -> NC.Request
    toRequest ci aReq = NC.defaultRequest
        { NC.method = ariMethod aReq
        , NC.secure = connectIsSecure ci
        , NC.host = encodeUtf8 $ connectHost ci
        , NC.port = connectPort ci
        , NC.path = B.intercalate "/" [adminPath, ariPath aReq]
        , NC.requestHeaders = ariHeaders aReq
        , NC.queryString = HT.renderQuery False $ ariQueryParams aReq
        , NC.requestBody = getRequestBody (ariPayload aReq)
        }
