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

module Network.Minio.Sign.V4 where

import qualified Conduit                       as C
import qualified Data.ByteString               as B
import qualified Data.ByteString.Base64        as Base64
import qualified Data.ByteString.Char8         as B8
import           Data.CaseInsensitive          (mk)
import qualified Data.CaseInsensitive          as CI
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Time                     as Time
import qualified Network.HTTP.Conduit          as NC
import           Network.HTTP.Types            (Header, parseQuery)
import qualified Network.HTTP.Types            as H
import           Text.Printf                   (printf)

import           Lib.Prelude

import           Network.Minio.Data.ByteString
import           Network.Minio.Data.Crypto
import           Network.Minio.Data.Time
import           Network.Minio.Errors

-- these headers are not included in the string to sign when signing a
-- request
ignoredHeaders :: Set.HashSet ByteString
ignoredHeaders = Set.fromList $ map CI.foldedCase
                 [ H.hAuthorization
                 , H.hContentType
                 , H.hUserAgent
                 ]

data SignV4Data = SignV4Data {
      sv4SignTime         :: UTCTime
    , sv4Scope            :: ByteString
    , sv4CanonicalRequest :: ByteString
    , sv4HeadersToSign    :: [(ByteString, ByteString)]
    , sv4Output           :: [(ByteString, ByteString)]
    , sv4StringToSign     :: ByteString
    , sv4SigningKey       :: ByteString
    } deriving (Show)

data SignParams = SignParams {
      spAccessKey   :: Text
    , spSecretKey   :: Text
    , spTimeStamp   :: UTCTime
    , spRegion      :: Maybe Text
    , spExpirySecs  :: Maybe Int
    , spPayloadHash :: Maybe ByteString
    } deriving (Show)

debugPrintSignV4Data :: SignV4Data -> IO ()
debugPrintSignV4Data (SignV4Data t s cr h2s o sts sk) = do
  B8.putStrLn "SignV4Data:"
  B8.putStr "Timestamp: " >> print t
  B8.putStr "Scope: " >> B8.putStrLn s
  B8.putStrLn "Canonical Request:"
  B8.putStrLn cr
  B8.putStr "Headers to Sign: " >> print h2s
  B8.putStr "Output: " >> print o
  B8.putStr "StringToSign: " >> B8.putStrLn sts
  B8.putStr "SigningKey: " >> printBytes sk
  B8.putStrLn "END of SignV4Data ========="
  where
    printBytes b = do
      mapM_ (\x -> B.putStr $ B.concat [show x,  " "]) $ B.unpack b
      B8.putStrLn ""

mkAuthHeader :: Text -> ByteString -> ByteString -> ByteString -> H.Header
mkAuthHeader accessKey scope signedHeaderKeys sign =
    let authValue = B.concat
                    [ "AWS4-HMAC-SHA256 Credential="
                    , toS accessKey
                    , "/"
                    , scope
                    , ", SignedHeaders="
                    , signedHeaderKeys
                    , ", Signature="
                    , sign
                    ]
    in (H.hAuthorization, authValue)

-- | Given SignParams and request details, including request method,
-- request path, headers, query params and payload hash, generates an
-- updated set of headers, including the x-amz-date header and the
-- Authorization header, which includes the signature.
--
-- For normal requests (i.e. without an expiry time), the output is
-- the list of headers to add to authenticate the request.
--
-- If `expiry` is not Nothing, it is assumed that a presigned request
-- is being created. The expiry is interpreted as an integer number of
-- seconds. The output will be the list of query-parameters to add to
-- the request.
signV4 :: SignParams -> NC.Request -> [(ByteString, ByteString)]
signV4 !sp !req =
  let
    region = fromMaybe "" $ spRegion sp
    ts = spTimeStamp sp
    scope = mkScope ts region
    accessKey = toS $ spAccessKey sp
    secretKey = toS $ spSecretKey sp
    expiry = spExpirySecs sp

    -- headers to be added to the request
    datePair = ("X-Amz-Date", awsTimeFormatBS ts)
    computedHeaders = NC.requestHeaders req ++
                      if isJust $ expiry
                      then []
                      else [(\(x, y) -> (mk x, y)) datePair]
    headersToSign = getHeadersToSign computedHeaders
    signedHeaderKeys = B.intercalate ";" $ sort $ map fst headersToSign

    -- query-parameters to be added before signing for presigned URLs
    -- (i.e. when `isJust expiry`)
    authQP = [ ("X-Amz-Algorithm", "AWS4-HMAC-SHA256")
             , ("X-Amz-Credential", B.concat [accessKey, "/", scope])
             , datePair
             , ("X-Amz-Expires", maybe "" show expiry)
             , ("X-Amz-SignedHeaders", signedHeaderKeys)
             ]
    finalQP = parseQuery (NC.queryString req)  ++
              if isJust expiry
              then (fmap . fmap) Just authQP
              else []

    -- 1. compute canonical request
    canonicalRequest = mkCanonicalRequest False sp
                       (NC.setQueryString finalQP req)
                       headersToSign

    -- 2. compute string to sign
    stringToSign = mkStringToSign ts scope canonicalRequest

    -- 3.1 compute signing key
    signingKey = mkSigningKey ts region secretKey

    -- 3.2 compute signature
    signature = computeSignature stringToSign signingKey

    -- 4. compute auth header
    authHeader = mkAuthHeader (spAccessKey sp) scope signedHeaderKeys signature

    -- finally compute output pairs
    sha256Hdr = ("x-amz-content-sha256",
                 fromMaybe "UNSIGNED-PAYLOAD" $ spPayloadHash sp)
    output = if isJust expiry
             then ("X-Amz-Signature", signature) : authQP
             else [(\(x, y) -> (CI.foldedCase x, y)) authHeader,
                   datePair, sha256Hdr]

  in output


mkScope :: UTCTime -> Text -> ByteString
mkScope ts region = B.intercalate "/"
  [ toS $ Time.formatTime Time.defaultTimeLocale "%Y%m%d" ts
  , toS region
  , "s3"
  , "aws4_request"
  ]

getHeadersToSign :: [Header] -> [(ByteString, ByteString)]
getHeadersToSign !h =
  filter ((\hdr -> not $ Set.member hdr ignoredHeaders) . fst) $
  map (\(x, y) -> (CI.foldedCase x, stripBS y)) h

mkCanonicalRequest :: Bool -> SignParams -> NC.Request -> [(ByteString, ByteString)]
                   -> ByteString
mkCanonicalRequest !isStreaming !sp !req !headersForSign =
  let
    canonicalQueryString = B.intercalate "&" $
      map (\(x, y) -> B.concat [x, "=", y]) $
      sort $ map (\(x, y) ->
                    (uriEncode True x, maybe "" (uriEncode True) y)) $
      (parseQuery $ NC.queryString req)

    sortedHeaders = sort headersForSign

    canonicalHeaders = B.concat $
      map (\(x, y) -> B.concat [x, ":", y, "\n"]) sortedHeaders

    signedHeaders = B.intercalate ";" $ map fst sortedHeaders

    payloadHashStr =
        if isStreaming
        then "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"
        else fromMaybe "UNSIGNED-PAYLOAD" $ spPayloadHash sp
  in
    B.intercalate "\n"
    [ NC.method req
    , uriEncode False $ NC.path req
    , canonicalQueryString
    , canonicalHeaders
    , signedHeaders
    , payloadHashStr
    ]

mkStringToSign :: UTCTime -> ByteString -> ByteString -> ByteString
mkStringToSign ts !scope !canonicalRequest = B.intercalate "\n"
                                             [ "AWS4-HMAC-SHA256"
                                             , awsTimeFormatBS ts
                                             , scope
                                             , hashSHA256 canonicalRequest
                                             ]

mkSigningKey :: UTCTime -> Text -> ByteString -> ByteString
mkSigningKey ts region !secretKey = hmacSHA256RawBS "aws4_request"
                                  . hmacSHA256RawBS "s3"
                                  . hmacSHA256RawBS (toS region)
                                  . hmacSHA256RawBS (awsDateFormatBS ts)
                                  $ B.concat ["AWS4", secretKey]

computeSignature :: ByteString -> ByteString -> ByteString
computeSignature !toSign !key = digestToBase16 $ hmacSHA256 toSign key

-- | Takes a validated Post Policy JSON bytestring, the signing time,
-- and ConnInfo and returns form-data for the POST upload containing
-- just the signature and the encoded post-policy.
signV4PostPolicy :: ByteString -> SignParams
                 -> Map.HashMap Text ByteString
signV4PostPolicy !postPolicyJSON !sp =
  let
    stringToSign = Base64.encode postPolicyJSON
    region = fromMaybe "" $ spRegion sp
    signingKey = mkSigningKey (spTimeStamp sp) region $ toS $ spSecretKey sp
    signature = computeSignature stringToSign signingKey
  in
    Map.fromList [ ("x-amz-signature", signature)
                 , ("policy", stringToSign)
                 ]

chunkSizeConstant :: Int
chunkSizeConstant = 64 * 1024

-- base16Len computes the number of bytes required to represent @n (> 0)@ in
-- hexadecimal.
base16Len :: Integral a => a -> Int
base16Len n | n == 0 = 0
            | otherwise = 1 + base16Len (n `div` 16)

signedStreamLength :: Int64 -> Int64
signedStreamLength dataLen =
  let
    chunkSzInt = fromIntegral chunkSizeConstant
    (numChunks, lastChunkLen) = quotRem dataLen chunkSzInt


    -- Structure of a chunk:
    --   string(IntHexBase(chunk-size)) + ";chunk-signature=" + signature + \r\n + chunk-data + \r\n
    encodedChunkLen csz = fromIntegral (base16Len csz) + 17 + 64 + 2 + csz + 2
    fullChunkSize = encodedChunkLen chunkSzInt
    lastChunkSize = bool 0 (encodedChunkLen lastChunkLen) $ lastChunkLen > 0
    finalChunkSize = 1 + 17 + 64 + 2 + 2
  in
    numChunks * fullChunkSize + lastChunkSize + finalChunkSize

signV4Stream :: Int64 -> SignParams -> NC.Request
             -> (C.ConduitT () ByteString (C.ResourceT IO) () -> NC.Request)
             -- -> ([Header], C.ConduitT () ByteString (C.ResourceT IO) () -> NC.RequestBody)
signV4Stream !payloadLength !sp !req =
  let
    ts = spTimeStamp sp

    addContentEncoding hs =
        let ceMay = headMay $ filter (\(x, _) -> x == "content-encoding") hs
        in case ceMay of
             Nothing      -> ("content-encoding", "aws-chunked") : hs
             Just (_, ce) -> ("content-encoding", ce <> ",aws-chunked") :
                             filter (\(x, _) -> x /= "content-encoding") hs

    -- headers to be added to the request
    datePair = ("X-Amz-Date", awsTimeFormatBS ts)
    computedHeaders = addContentEncoding $
                      datePair : NC.requestHeaders req

    -- headers specific to streaming signature
    signedContentLength = signedStreamLength payloadLength
    streamingHeaders :: [Header]
    streamingHeaders =
        [ ("x-amz-decoded-content-length", show payloadLength)
        , ("content-length", show signedContentLength )
        , ("x-amz-content-sha256", "STREAMING-AWS4-HMAC-SHA256-PAYLOAD")
        ]
    headersToSign = getHeadersToSign $ computedHeaders ++ streamingHeaders
    signedHeaderKeys = B.intercalate ";" $ sort $ map fst headersToSign
    finalQP = parseQuery (NC.queryString req)

    -- 1. Compute Seed Signature
    -- 1.1 Canonical Request
    canonicalReq = mkCanonicalRequest True sp
                   (NC.setQueryString finalQP req)
                   headersToSign

    region = fromMaybe "" $ spRegion sp
    scope = mkScope ts region
    accessKey = spAccessKey sp
    secretKey = spSecretKey sp

    -- 1.2 String toSign
    stringToSign = mkStringToSign ts scope canonicalReq

    -- 1.3 Compute signature
    -- 1.3.1 compute signing key
    signingKey = mkSigningKey ts region $ toS secretKey

    -- 1.3.2 Compute signature
    seedSignature = computeSignature stringToSign signingKey

    -- 1.3.3 Compute Auth Header
    authHeader = mkAuthHeader accessKey scope signedHeaderKeys seedSignature

    -- 1.4 Updated headers for the request
    finalReqHeaders = authHeader : (computedHeaders ++ streamingHeaders)
    -- headersToAdd = authHeader : datePair : streamingHeaders

    toHexStr n = B8.pack $ printf "%x" n

    (numParts, lastPSize) = payloadLength `quotRem` fromIntegral chunkSizeConstant

    -- Function to compute string to sign for each chunk.
    chunkStrToSign prevSign currChunkHash =
        B.intercalate "\n"
        [ "AWS4-HMAC-SHA256-PAYLOAD"
        , awsTimeFormatBS ts
        , scope
        , prevSign
        , hashSHA256 ""
        , currChunkHash
        ]

    -- Read n byte from upstream and return a strict bytestring.
    mustTakeN n = do
        bs <- toS <$> (C.takeCE n C..| C.sinkLazy)
        when (B.length bs /= n) $
            throwIO MErrVStreamingBodyUnexpectedEOF
        return bs

    signerConduit n lps prevSign =
         -- First case encodes a full chunk of length
         -- 'chunkSizeConstant'.
      if | n > 0 -> do
               bs <- mustTakeN chunkSizeConstant
               let strToSign = chunkStrToSign prevSign (hashSHA256 bs)
                   nextSign = computeSignature strToSign signingKey
                   chunkBS = toHexStr chunkSizeConstant
                          <> ";chunk-signature="
                          <> nextSign <> "\r\n" <> bs <> "\r\n"
               C.yield chunkBS
               signerConduit (n-1) lps nextSign

         -- Second case encodes the last chunk which is smaller than
         -- 'chunkSizeConstant'
         | lps > 0 -> do
               bs <- mustTakeN $ fromIntegral lps
               let strToSign = chunkStrToSign prevSign (hashSHA256 bs)
                   nextSign = computeSignature strToSign signingKey
                   chunkBS = toHexStr lps <> ";chunk-signature="
                          <> nextSign <> "\r\n" <> bs <> "\r\n"
               C.yield chunkBS
               signerConduit 0 0 nextSign

         -- Last case encodes the final signature chunk that has no
         -- data.
         | otherwise -> do
               let strToSign = chunkStrToSign prevSign (hashSHA256 "")
                   nextSign = computeSignature strToSign signingKey
                   lastChunkBS = "0;chunk-signature=" <> nextSign <> "\r\n\r\n"
               C.yield lastChunkBS
  in
    \src -> req { NC.requestHeaders = finalReqHeaders
                , NC.requestBody =
                  NC.requestBodySource signedContentLength $
                  src C..| signerConduit numParts lastPSize seedSignature
                }
