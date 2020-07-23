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

module Network.Minio.PutObject
  (
    putObjectInternal
  , ObjectData(..)
  , selectPartSizes
  ) where


import           Conduit                  (takeC)
import qualified Conduit                  as C
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List        as CL
import qualified Data.List                as List


import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.S3API
import           Network.Minio.Utils


-- | A data-type to represent the source data for an object. A
-- file-path or a producer-conduit may be provided.
--
-- For files, a size may be provided - this is useful in cases when
-- the file size cannot be automatically determined or if only some
-- prefix of the file is desired.
--
-- For streams also, a size may be provided. This is useful to limit
-- the input - if it is not provided, upload will continue until the
-- stream ends or the object reaches `maxObjectSize` size.
data ObjectData m
  = ODFile FilePath (Maybe Int64) -- ^ Takes filepath and optional
                                  -- size.
  | ODStream (C.ConduitM () ByteString m ()) (Maybe Int64) -- ^ Pass
                                                           -- size
                                                           -- (bytes)
                                                           -- if
                                                           -- known.

-- | Put an object from ObjectData. This high-level API handles
-- objects of all sizes, and even if the object size is unknown.
putObjectInternal :: Bucket -> Object -> PutObjectOptions
                  -> ObjectData Minio -> Minio ETag
putObjectInternal b o opts (ODStream src sizeMay) = do
  case sizeMay of
    -- unable to get size, so assume non-seekable file
    Nothing -> sequentialMultipartUpload b o opts Nothing src

    -- got file size, so check for single/multipart upload
    Just size ->
      if | size <= 64 * oneMiB -> do
             bs <- C.runConduit $ src C..| takeC (fromIntegral size) C..| CB.sinkLbs
             putObjectSingle' b o (pooToHeaders opts) $ LBS.toStrict bs
         | size > maxObjectSize -> throwIO $ MErrVPutSizeExceeded size
         | otherwise -> sequentialMultipartUpload b o opts (Just size) src

putObjectInternal b o opts (ODFile fp sizeMay) = do
  hResE <- withNewHandle fp $ \h ->
    liftM2 (,) (isHandleSeekable h) (getFileSize h)

  (isSeekable, handleSizeMay) <- either (const $ return (False, Nothing)) return
                                 hResE

  -- prefer given size to queried size.
  let finalSizeMay = listToMaybe $ catMaybes [sizeMay, handleSizeMay]

  case finalSizeMay of
    -- unable to get size, so assume non-seekable file
    Nothing -> sequentialMultipartUpload b o opts Nothing $ CB.sourceFile fp

    -- got file size, so check for single/multipart upload
    Just size ->
      if | size <= 64 * oneMiB -> either throwIO return =<<
           withNewHandle fp (\h -> putObjectSingle b o (pooToHeaders opts) h 0 size)
         | size > maxObjectSize -> throwIO $ MErrVPutSizeExceeded size
         | isSeekable -> parallelMultipartUpload b o opts fp size
         | otherwise -> sequentialMultipartUpload b o opts (Just size) $
                        CB.sourceFile fp

parallelMultipartUpload :: Bucket -> Object -> PutObjectOptions
                        -> FilePath -> Int64 -> Minio ETag
parallelMultipartUpload b o opts filePath size = do
  -- get a new upload id.
  uploadId <- newMultipartUpload b o (pooToHeaders opts)

  let partSizeInfo = selectPartSizes size

  let threads = fromMaybe 10 $ pooNumThreads opts

  -- perform upload with 'threads' threads
  uploadedPartsE <- limitedMapConcurrently (fromIntegral threads)
                    (uploadPart uploadId) partSizeInfo

  -- if there were any errors, rethrow exception.
  mapM_ throwIO $ lefts uploadedPartsE

  -- if we get here, all parts were successfully uploaded.
  completeMultipartUpload b o uploadId $ rights uploadedPartsE

  where
    uploadPart uploadId (partNum, offset, sz) =
      withNewHandle filePath $ \h -> do
        let payload = PayloadH h offset sz
        putObjectPart b o uploadId partNum [] payload

-- | Upload multipart object from conduit source sequentially
sequentialMultipartUpload :: Bucket -> Object -> PutObjectOptions
                          -> Maybe Int64
                          -> C.ConduitM () ByteString Minio ()
                          -> Minio ETag
sequentialMultipartUpload b o opts sizeMay src = do
  -- get a new upload id.
  uploadId <- newMultipartUpload b o (pooToHeaders opts)

  -- upload parts in loop
  let partSizes = selectPartSizes $ maybe maxObjectSize identity sizeMay
      (pnums, _, sizes) = List.unzip3 partSizes
  uploadedParts <- C.runConduit
                 $ src
              C..| chunkBSConduit (map fromIntegral sizes)
              C..| CL.map PayloadBS
              C..| uploadPart' uploadId pnums
              C..| CC.sinkList

  -- complete multipart upload
  completeMultipartUpload b o uploadId uploadedParts

  where
    uploadPart' _ [] = return ()
    uploadPart' uid (pn:pns) = do
      payloadMay <- C.await
      case payloadMay of
        Nothing -> return ()
        Just payload -> do pinfo <- lift $ putObjectPart b o uid pn [] payload
                           C.yield pinfo
                           uploadPart' uid pns
