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

module Network.Minio.SelectAPI
  (

  -- | The `selectObjectContent` allows querying CSV, JSON or Parquet
  -- format objects in AWS S3 and in MinIO using SQL Select
  -- statements. This allows significant reduction of data transfer
  -- from object storage for computation-intensive tasks, as relevant
  -- data is filtered close to the storage.

    selectObjectContent

  , SelectRequest
  , selectRequest

  -- *** Input Serialization

  , InputSerialization
  , defaultCsvInput
  , linesJsonInput
  , documentJsonInput
  , defaultParquetInput
  , setInputCSVProps

  , CompressionType(..)
  , setInputCompressionType

  -- *** CSV Format details

  -- | CSV format options such as delimiters and quote characters are
  -- specified using using the functions below. Options are combined
  -- monoidally.

  , CSVProp
  , recordDelimiter
  , fieldDelimiter
  , quoteCharacter
  , quoteEscapeCharacter
  , commentCharacter
  , allowQuotedRecordDelimiter
  , FileHeaderInfo(..)
  , fileHeaderInfo
  , QuoteFields(..)
  , quoteFields

  -- *** Output Serialization

  , OutputSerialization
  , defaultCsvOutput
  , defaultJsonOutput
  , outputCSVFromProps
  , outputJSONFromRecordDelimiter

  -- *** Progress messages

  , setRequestProgressEnabled

  -- *** Interpreting Select output

  -- | The conduit returned by `selectObjectContent` returns values of
  -- the `EventMessage` data type. This returns the query output
  -- messages formatted according to the chosen output serialization,
  -- interleaved with progress messages (if enabled by
  -- `setRequestProgressEnabled`), and at the end a statistics
  -- message.
  --
  -- If the application is interested in only the payload, then
  -- `getPayloadBytes` can be used. For example to simply print the
  -- payload to stdout:
  --
  -- > resultConduit <- selectObjectContent bucket object mySelectRequest
  -- > runConduit $ resultConduit .| getPayloadBytes .| stdoutC
  --
  -- Note that runConduit, the connect operator (.|) and stdoutC are
  -- all from the "conduit" package.

  , getPayloadBytes
  , EventMessage(..)
  , Progress(..)
  , Stats
  ) where

import           Conduit                    ((.|))
import qualified Conduit                    as C
import qualified Data.Binary                as Bin
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import           Data.Digest.CRC32          (crc32, crc32Update)
import qualified Network.HTTP.Conduit       as NC
import qualified Network.HTTP.Types         as HT
import           UnliftIO                   (MonadUnliftIO)

import           Lib.Prelude

import           Network.Minio.API
import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.Utils
import           Network.Minio.XmlGenerator
import           Network.Minio.XmlParser

data EventStreamException = ESEPreludeCRCFailed
                          | ESEMessageCRCFailed
                          | ESEUnexpectedEndOfStream
                          | ESEDecodeFail [Char]
                          | ESEInvalidHeaderType
                          | ESEInvalidHeaderValueType
                          | ESEInvalidMessageType
    deriving (Eq, Show)

instance Exception EventStreamException

-- chunkSize in bytes is 32KiB
chunkSize :: Int
chunkSize = 32 * 1024

parseBinary :: Bin.Binary a => ByteString -> IO a
parseBinary b = do
    case Bin.decodeOrFail $ LB.fromStrict b of
      Left (_, _, msg) -> throwIO $ ESEDecodeFail msg
      Right (_, _, r)  -> return r

bytesToHeaderName :: Text -> IO MsgHeaderName
bytesToHeaderName t = case t of
  ":message-type"  -> return MessageType
  ":event-type"    -> return EventType
  ":content-type"  -> return ContentType
  ":error-code"    -> return ErrorCode
  ":error-message" -> return ErrorMessage
  _                -> throwIO ESEInvalidHeaderType

parseHeaders :: MonadUnliftIO m
             => Word32 -> C.ConduitM ByteString a m [MessageHeader]
parseHeaders 0 = return []
parseHeaders hdrLen = do
    bs1 <- readNBytes 1
    n :: Word8 <- liftIO $ parseBinary bs1

    headerKeyBytes <- readNBytes $ fromIntegral n
    let headerKey = decodeUtf8Lenient headerKeyBytes
    headerName <- liftIO $ bytesToHeaderName headerKey

    bs2 <- readNBytes 1
    headerValueType :: Word8 <- liftIO $ parseBinary bs2
    when (headerValueType /= 7) $ throwIO ESEInvalidHeaderValueType

    bs3 <- readNBytes 2
    vLen :: Word16 <- liftIO $ parseBinary bs3
    headerValueBytes <- readNBytes $ fromIntegral vLen
    let headerValue = decodeUtf8Lenient headerValueBytes
        m = (headerName, headerValue)
        k = 1 + fromIntegral n + 1 + 2 + fromIntegral vLen

    ms <- parseHeaders (hdrLen - k)
    return (m:ms)

-- readNBytes returns N bytes read from the string and throws an
-- exception if N bytes are not present on the stream.
readNBytes :: MonadUnliftIO m => Int -> C.ConduitM ByteString a m ByteString
readNBytes n = do
    b <- LB.toStrict <$> (C.takeCE n .| C.sinkLazy)
    if B.length b /= n
        then throwIO ESEUnexpectedEndOfStream
        else return b

crcCheck :: MonadUnliftIO m
         => C.ConduitM ByteString ByteString m ()
crcCheck = do
    b <- readNBytes 12
    n :: Word32 <- liftIO $ parseBinary $ B.take 4 b
    preludeCRC :: Word32 <- liftIO $ parseBinary $ B.drop 8 b
    when (crc32 (B.take 8 b) /= preludeCRC) $
        throwIO ESEPreludeCRCFailed

    -- we do not yield the checksum
    C.yield $ B.take 8 b

    -- 12 bytes have been read off the current message. Now read the
    -- next (n-12)-4 bytes and accumulate the checksum, and yield it.
    let startCrc = crc32 b
    finalCrc <- accumulateYield (fromIntegral n-16) startCrc

    bs <- readNBytes 4
    expectedCrc :: Word32 <- liftIO $ parseBinary bs

    when (finalCrc /= expectedCrc) $
        throwIO ESEMessageCRCFailed

    -- we unconditionally recurse - downstream figures out when to
    -- quit reading the stream
    crcCheck
  where
    accumulateYield n checkSum = do
        let toRead = min n chunkSize
        b <- readNBytes toRead
        let c' = crc32Update checkSum b
            n' = n - B.length b
        C.yield b
        if n' > 0
            then accumulateYield n' c'
            else return c'

handleMessage :: MonadUnliftIO m => C.ConduitT ByteString EventMessage m ()
handleMessage = do
    b1 <- readNBytes 4
    msgLen :: Word32 <- liftIO $ parseBinary b1

    b2 <- readNBytes 4
    hdrLen :: Word32 <- liftIO $ parseBinary b2

    hs <- parseHeaders hdrLen

    let payloadLen = msgLen - hdrLen - 16
        getHdrVal h = fmap snd . headMay . filter ((h ==) . fst)
        eventHdrValue = getHdrVal EventType hs
        msgHdrValue = getHdrVal MessageType hs
        errCode = getHdrVal ErrorCode hs
        errMsg = getHdrVal ErrorMessage hs

    case msgHdrValue of
      Just "event" -> do
          case eventHdrValue of
            Just "Records" -> passThrough $ fromIntegral payloadLen
            Just "Cont" -> return ()
            Just "Progress" -> do
                bs <- readNBytes $ fromIntegral payloadLen
                progress <- parseSelectProgress bs
                C.yield $ ProgressEventMessage progress
            Just "Stats" -> do
                bs <- readNBytes $ fromIntegral payloadLen
                stats <- parseSelectProgress bs
                C.yield $ StatsEventMessage stats
            Just "End" -> return ()
            _ -> throwIO ESEInvalidMessageType
          when (eventHdrValue /= Just "End") handleMessage

      Just "error" -> do
          let reqMsgMay = RequestLevelErrorMessage <$> errCode <*> errMsg
          maybe (throwIO ESEInvalidMessageType) C.yield reqMsgMay

      _ -> throwIO ESEInvalidMessageType

  where
    passThrough 0 = return ()
    passThrough n = do
        let c = min n chunkSize
        b <- readNBytes c
        C.yield $ RecordPayloadEventMessage b
        passThrough $ n - B.length b


selectProtoConduit :: MonadUnliftIO m
                   => C.ConduitT ByteString EventMessage m ()
selectProtoConduit = crcCheck .| handleMessage

-- | selectObjectContent calls the SelectRequest on the given
-- object. It returns a Conduit of event messages that can be consumed
-- by the client.
selectObjectContent :: Bucket -> Object -> SelectRequest
                    -> Minio (C.ConduitT () EventMessage Minio ())
selectObjectContent b o r = do
    let reqInfo = defaultS3ReqInfo { riMethod = HT.methodPost
                                   , riBucket = Just b
                                   , riObject = Just o
                                   , riPayload = PayloadBS $ mkSelectRequest r
                                   , riNeedsLocation = False
                                   , riQueryParams = [("select", Nothing), ("select-type", Just "2")]
                                   }
    --print $ mkSelectRequest r
    resp <- mkStreamRequest reqInfo
    return $ NC.responseBody resp .| selectProtoConduit

-- | A helper conduit that returns only the record payload bytes.
getPayloadBytes :: MonadIO m => C.ConduitT EventMessage ByteString m ()
getPayloadBytes = do
    evM <- C.await
    case evM of
      Just v -> do
          case v of
            RecordPayloadEventMessage b  -> C.yield b
            RequestLevelErrorMessage c m -> liftIO $ throwIO $ SelectErr c m
            _                            -> return ()
          getPayloadBytes
      Nothing -> return ()
