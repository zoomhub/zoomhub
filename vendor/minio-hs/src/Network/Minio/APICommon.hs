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

module Network.Minio.APICommon where

import qualified Conduit                   as C
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LB
import           Data.Conduit.Binary       (sourceHandleRange)
import qualified Network.HTTP.Conduit      as NC
import qualified Network.HTTP.Types        as HT

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Data.Crypto
import           Network.Minio.Errors

sha256Header :: ByteString -> HT.Header
sha256Header = ("x-amz-content-sha256", )

-- | This function throws an error if the payload is a conduit (as it
-- will not be possible to re-read the conduit after it is consumed).
getPayloadSHA256Hash :: Payload -> Minio ByteString
getPayloadSHA256Hash (PayloadBS bs) = return $ hashSHA256 bs
getPayloadSHA256Hash (PayloadH h off size) = hashSHA256FromSource $
  sourceHandleRange h
    (return . fromIntegral $ off)
    (return . fromIntegral $ size)
getPayloadSHA256Hash (PayloadC _ _) = throwIO MErrVUnexpectedPayload

getRequestBody :: Payload -> NC.RequestBody
getRequestBody (PayloadBS bs) = NC.RequestBodyBS bs
getRequestBody (PayloadH h off size) =
  NC.requestBodySource (fromIntegral size) $
    sourceHandleRange h
      (return . fromIntegral $ off)
      (return . fromIntegral $ size)
getRequestBody (PayloadC n src) = NC.requestBodySource n src

mkStreamingPayload :: Payload -> Payload
mkStreamingPayload payload =
  case payload of
    PayloadBS bs ->
        PayloadC (fromIntegral $ BS.length bs)
        (C.sourceLazy $ LB.fromStrict bs)
    PayloadH h off len ->
        PayloadC len $ sourceHandleRange h
        (return . fromIntegral $ off)
        (return . fromIntegral $ len)
    _ -> payload

isStreamingPayload :: Payload -> Bool
isStreamingPayload (PayloadC _ _) = True
isStreamingPayload _              = False
