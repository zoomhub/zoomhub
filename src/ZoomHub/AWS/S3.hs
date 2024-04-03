{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.AWS.S3
  ( presignPOSTPolicy,
  )
where

import qualified Amazonka as AWS
import Amazonka.Data (ToText (toText))
import qualified Amazonka.Data as AWS
import qualified Amazonka.S3 as S3
import qualified Amazonka.Sign.V4.Base as AWS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as H
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import qualified Data.Time as Time
import Data.Time.Clock (UTCTime (UTCTime, utctDay), getCurrentTime)
import ZoomHub.AWS.S3.POSTPolicy (POSTPolicy)
import qualified ZoomHub.AWS.S3.POSTPolicy as POSTPolicy
import qualified ZoomHub.AWS.S3.POSTPolicy.Condition as POSTPolicyCondition
import ZoomHub.Utils (lenientDecodeUtf8)

presignPOSTPolicy ::
  AWS.AccessKey ->
  AWS.SecretKey ->
  AWS.Region ->
  POSTPolicy ->
  IO (H.HashMap Text ByteString)
presignPOSTPolicy accessKey secretKey region policy = do
  signTime <- getCurrentTime
  let expiration = POSTPolicy.expiration policy
      date = UTCTime (utctDay expiration) 0

  let scope =
        AWS.credentialScope
          S3.defaultService
          ( AWS.Endpoint
              { AWS.scope = encodeUtf8 (AWS.fromRegion region),
                -- unused dummy values
                AWS.basePath = AWS.rawPath ("" :: ByteString),
                AWS.host = "",
                AWS.secure = True,
                AWS.port = 0
              }
          )
          date
  let extraConditions =
        [ POSTPolicyCondition.Equals "x-amz-date" (T.pack $ awsTimeFormat signTime),
          POSTPolicyCondition.Equals "x-amz-algorithm" "AWS4-HMAC-SHA256",
          POSTPolicyCondition.Equals
            "x-amz-credential"
            ( T.intercalate "/" [toText accessKey, decodeUtf8Lenient $ AWS.toBS scope]
            )
        ]
      policyWithCredentials =
        policy
          { POSTPolicy.conditions = POSTPolicy.conditions policy <> extraConditions
          }

      mkPair (POSTPolicyCondition.StartsWith k v) = Just (k, v)
      mkPair (POSTPolicyCondition.Equals k v) = Just (k, v)
      mkPair (POSTPolicyCondition.Range {}) = Nothing

      formFromPolicy =
        H.map encodeUtf8 $
          H.fromList $
            mapMaybe
              mkPair
              (POSTPolicy.conditions policyWithCredentials)

  let stringToSign = AWS.Tag . Base64.encodeBase64' . POSTPolicy.encode $ policyWithCredentials
      signature = AWS.signature secretKey scope stringToSign
  pure $
    H.fromList
      [ ("x-amz-signature", AWS.toBS signature),
        ("policy", AWS.toBS stringToSign)
      ]
      `H.union` formFromPolicy

awsTimeFormat :: UTCTime -> String
awsTimeFormat = Time.formatTime Time.defaultTimeLocale "%Y%m%dT%H%M%SZ"
