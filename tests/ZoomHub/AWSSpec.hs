{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ZoomHub.AWSSpec where

import qualified Amazonka as AWS
import qualified Amazonka.Data as AWS
import qualified Amazonka.S3 as S3
import qualified Amazonka.Sign.V4.Base as AWS
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime))
import Test.Hspec (Spec, describe, it, shouldBe)

-- Tests whether we can use `amazonka` instead of `minio-hs` for signing S3 POST
-- policies:
-- https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-post-example.html
spec :: Spec
spec =
  describe "AWS" $
    describe "POST policy" $ do
      it "returns correct Base64 encoding" $ do
        payload <- B.readFile "tests/ZoomHub/payload.txt"
        let stringToSign = Base64.encodeBase64' payload
        stringToSign `shouldBe` "eyAiZXhwaXJhdGlvbiI6ICIyMDE1LTEyLTMwVDEyOjAwOjAwLjAwMFoiLA0KICAiY29uZGl0aW9ucyI6IFsNCiAgICB7ImJ1Y2tldCI6ICJzaWd2NGV4YW1wbGVidWNrZXQifSwNCiAgICBbInN0YXJ0cy13aXRoIiwgIiRrZXkiLCAidXNlci91c2VyMS8iXSwNCiAgICB7ImFjbCI6ICJwdWJsaWMtcmVhZCJ9LA0KICAgIHsic3VjY2Vzc19hY3Rpb25fcmVkaXJlY3QiOiAiaHR0cDovL3NpZ3Y0ZXhhbXBsZWJ1Y2tldC5zMy5hbWF6b25hd3MuY29tL3N1Y2Nlc3NmdWxfdXBsb2FkLmh0bWwifSwNCiAgICBbInN0YXJ0cy13aXRoIiwgIiRDb250ZW50LVR5cGUiLCAiaW1hZ2UvIl0sDQogICAgeyJ4LWFtei1tZXRhLXV1aWQiOiAiMTQzNjUxMjM2NTEyNzQifSwNCiAgICB7IngtYW16LXNlcnZlci1zaWRlLWVuY3J5cHRpb24iOiAiQUVTMjU2In0sDQogICAgWyJzdGFydHMtd2l0aCIsICIkeC1hbXotbWV0YS10YWciLCAiIl0sDQoNCiAgICB7IngtYW16LWNyZWRlbnRpYWwiOiAiQUtJQUlPU0ZPRE5ON0VYQU1QTEUvMjAxNTEyMjkvdXMtZWFzdC0xL3MzL2F3czRfcmVxdWVzdCJ9LA0KICAgIHsieC1hbXotYWxnb3JpdGhtIjogIkFXUzQtSE1BQy1TSEEyNTYifSwNCiAgICB7IngtYW16LWRhdGUiOiAiMjAxNTEyMjlUMDAwMDAwWiIgfQ0KICBdDQp9"

      it "returns correct signature" $ do
        payload <- B.readFile "tests/ZoomHub/payload.txt"
        let stringToSign = AWS.Tag $ Base64.encodeBase64' payload

        -- See: https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-post-example.html
        let secretKey = AWS.SecretKey "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
            date = UTCTime (fromGregorian 2015 12 29) 0

        let scope =
              AWS.credentialScope
                S3.defaultService
                ( AWS.Endpoint
                    { AWS.scope = "us-east-1",
                      -- unused dummy values
                      AWS.basePath = AWS.rawPath ("" :: ByteString),
                      AWS.host = "",
                      AWS.secure = True,
                      AWS.port = 0
                    }
                )
                date

        let signature = AWS.signature secretKey scope stringToSign
        AWS.untag signature `shouldBe` "8afdbf4008c03f22c2cd3cdb72e4afbb1f6a588f3255ac628749a66d7f09699e"
