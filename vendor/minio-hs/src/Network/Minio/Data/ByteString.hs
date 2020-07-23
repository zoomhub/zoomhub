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

{-# LANGUAGE FlexibleInstances #-}
module Network.Minio.Data.ByteString
  (
    stripBS
  , UriEncodable(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as LB
import           Data.Char (isSpace, toUpper, isAsciiUpper, isAsciiLower, isDigit)
import qualified Data.Text as T
import           Numeric (showHex)

import           Lib.Prelude

stripBS :: ByteString -> ByteString
stripBS = BC8.dropWhile isSpace . fst . BC8.spanEnd isSpace

class UriEncodable s where
  uriEncode :: Bool -> s -> ByteString

instance UriEncodable [Char] where
  uriEncode encodeSlash payload =
    LB.toStrict $ BB.toLazyByteString $ mconcat $
    map (`uriEncodeChar` encodeSlash) payload

instance UriEncodable ByteString where
  -- assumes that uriEncode is passed ASCII encoded strings.
  uriEncode encodeSlash bs =
    uriEncode encodeSlash $ BC8.unpack bs

instance UriEncodable Text where
  uriEncode encodeSlash txt =
    uriEncode encodeSlash $ T.unpack txt

-- | URI encode a char according to AWS S3 signing rules - see
-- UriEncode() at
-- https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
uriEncodeChar :: Char -> Bool -> BB.Builder
uriEncodeChar '/' True = BB.byteString "%2F"
uriEncodeChar '/' False = BB.char7 '/'
uriEncodeChar ch _
  | isAsciiUpper ch
    || isAsciiLower ch
    || isDigit ch
    || (ch == '_')
    || (ch == '-')
    || (ch == '.')
    || (ch == '~') = BB.char7 ch
  | otherwise = mconcat $ map f $ B.unpack $ encodeUtf8 $ T.singleton ch
  where
    f :: Word8 -> BB.Builder
    f n = BB.char7 '%' <> BB.string7 hexStr
      where
        hexStr = map toUpper $ showHex q $ showHex r ""
        (q, r) = divMod (fromIntegral n) (16::Word8)
