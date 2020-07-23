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

module Network.Minio.CopyObject where

import qualified Data.List            as List

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.S3API
import           Network.Minio.Utils


-- | Copy an object using single or multipart copy strategy.
copyObjectInternal :: Bucket -> Object -> SourceInfo
                   -> Minio ETag
copyObjectInternal b' o srcInfo = do
  let sBucket = srcBucket srcInfo
      sObject = srcObject srcInfo

  -- get source object size with a head request
  oi <- headObject sBucket sObject []
  let srcSize = oiSize oi

  -- check that byte offsets are valid if specified in cps
  let rangeMay = srcRange srcInfo
      range = maybe (0, srcSize) identity rangeMay
      startOffset = fst range
      endOffset = snd range

  when (isJust rangeMay &&
        or [startOffset < 0, endOffset < startOffset,
            endOffset >= fromIntegral srcSize]) $
    throwIO $ MErrVInvalidSrcObjByteRange range

  -- 1. If sz > 64MiB (minPartSize) use multipart copy, OR
  -- 2. If startOffset /= 0 use multipart copy
  let destSize = (\(a, b) -> b - a + 1 ) $
                 maybe (0, srcSize - 1) identity rangeMay

  if destSize > minPartSize || (endOffset - startOffset + 1 /= srcSize)
    then multiPartCopyObject b' o srcInfo srcSize

    else fst <$> copyObjectSingle b' o srcInfo{srcRange = Nothing} []

-- | Given the input byte range of the source object, compute the
-- splits for a multipart copy object procedure. Minimum part size
-- used is minPartSize.
selectCopyRanges :: (Int64, Int64) -> [(PartNumber, (Int64, Int64))]
selectCopyRanges (st, end) = zip pns $
  map (\(x, y) -> (st + x, st + x + y - 1)) $ zip startOffsets partSizes
  where
    size = end - st + 1
    (pns, startOffsets, partSizes) = List.unzip3 $ selectPartSizes size

-- | Perform a multipart copy object action. Since we cannot verify
-- existing parts based on the source object, there is no resuming
-- copy action support.
multiPartCopyObject :: Bucket -> Object -> SourceInfo -> Int64
                    -> Minio ETag
multiPartCopyObject b o cps srcSize = do
  uid <- newMultipartUpload b o []

  let byteRange = maybe (0, fromIntegral $ srcSize - 1) identity $ srcRange cps
      partRanges = selectCopyRanges byteRange
      partSources = map (\(x, (start, end)) -> (x, cps {srcRange = Just (start, end) }))
                    partRanges
      dstInfo = defaultDestinationInfo { dstBucket = b, dstObject = o}

  copiedParts <- limitedMapConcurrently 10
                 (\(pn, cps') -> do
                     (etag, _) <- copyObjectPart dstInfo cps' uid pn []
                     return (pn, etag)
                 )
                 partSources

  completeMultipartUpload b o uid copiedParts
