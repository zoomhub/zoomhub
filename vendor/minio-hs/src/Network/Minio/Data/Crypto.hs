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

module Network.Minio.Data.Crypto
  (
    hashSHA256
  , hashSHA256FromSource

  , hashMD5
  , hashMD5ToBase64
  , hashMD5FromSource

  , hmacSHA256
  , hmacSHA256RawBS
  , digestToBS
  , digestToBase16

  , encodeToBase64
  ) where

import           Crypto.Hash             (Digest, MD5 (..), SHA256 (..),
                                          hashWith)
import           Crypto.Hash.Conduit     (sinkHash)
import           Crypto.MAC.HMAC         (HMAC, hmac)
import           Data.ByteArray          (ByteArrayAccess, convert)
import           Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import qualified Data.Conduit            as C

import           Lib.Prelude

hashSHA256 :: ByteString -> ByteString
hashSHA256 = digestToBase16 . hashWith SHA256

hashSHA256FromSource :: Monad m => C.ConduitM () ByteString m () -> m ByteString
hashSHA256FromSource src = do
  digest <- C.connect src sinkSHA256Hash
  return $ digestToBase16 digest
  where
    -- To help with type inference
    sinkSHA256Hash :: Monad m => C.ConduitM ByteString Void m (Digest SHA256)
    sinkSHA256Hash = sinkHash

-- Returns MD5 hash hex encoded.
hashMD5 :: ByteString -> ByteString
hashMD5 = digestToBase16 . hashWith MD5

hashMD5FromSource :: Monad m => C.ConduitM () ByteString m () -> m ByteString
hashMD5FromSource src = do
  digest <- C.connect src sinkMD5Hash
  return $ digestToBase16 digest
  where
    -- To help with type inference
    sinkMD5Hash :: Monad m => C.ConduitM ByteString Void m (Digest MD5)
    sinkMD5Hash = sinkHash

hmacSHA256 :: ByteString -> ByteString -> HMAC SHA256
hmacSHA256 message key = hmac key message

hmacSHA256RawBS :: ByteString -> ByteString -> ByteString
hmacSHA256RawBS message key = convert $ hmacSHA256 message key

digestToBS :: ByteArrayAccess a => a -> ByteString
digestToBS = convert

digestToBase16 :: ByteArrayAccess a => a -> ByteString
digestToBase16 = convertToBase Base16

-- Returns MD5 hash base 64 encoded.
hashMD5ToBase64 :: ByteArrayAccess a => a -> ByteString
hashMD5ToBase64 = convertToBase Base64 . hashWith MD5

encodeToBase64 :: ByteArrayAccess a => a -> ByteString
encodeToBase64 = convertToBase Base64
