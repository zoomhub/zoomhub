{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- Adapted from `minio-hs` under Apache License 2
module ZoomHub.AWS.S3.POSTPolicy
  ( POSTPolicy,
    mkPOSTPolicy,
    encode,
    expiration,
    conditions,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import ZoomHub.AWS.S3.POSTPolicy.Condition (Condition)
import qualified ZoomHub.AWS.S3.POSTPolicy.Condition as Condition
import ZoomHub.AWS.S3.POSTPolicy.Error (Error)
import qualified ZoomHub.AWS.S3.POSTPolicy.Error as Error

-- | A `POSTPolicy` is required to perform uploads via browser forms.
data POSTPolicy = POSTPolicy
  { expiration :: UTCTime,
    conditions :: [Condition]
  }
  deriving stock (Show, Eq)

instance JSON.ToJSON POSTPolicy where
  toJSON (POSTPolicy e c) =
    JSON.object
      [ "expiration" .= iso8601TimeFormat e,
        "conditions" .= c
      ]
  toEncoding (POSTPolicy e c) =
    JSON.pairs
      ( "expiration" .= iso8601TimeFormat e
          <> "conditions" .= c
      )

-- | This function creates a POSTPolicy after validating its
-- arguments.
mkPOSTPolicy :: UTCTime -> [Condition] -> Either Error POSTPolicy
mkPOSTPolicy expirationTime conditions'
  -- object name condition must be present
  | not $ any (keyEquals "key") conditions' =
      Left Error.KeyNotSpecified
  -- bucket name condition must be present
  | not $ any (keyEquals "bucket") conditions' =
      Left Error.BucketNotSpecified
  -- a condition with an empty key is invalid
  | any (keyEquals "") conditions' || any isEmptyRangeKey conditions' =
      Left Error.ConditionKeyEmpty
  -- invalid range check
  | any isInvalidRange conditions' =
      Left Error.RangeInvalid
  -- all good!
  | otherwise =
      return $ POSTPolicy expirationTime conditions'
  where
    keyEquals k' (Condition.StartsWith k _) = k == k'
    keyEquals k' (Condition.Equals k _) = k == k'
    keyEquals _ _ = False

    isEmptyRangeKey (Condition.Range k _ _) = k == ""
    isEmptyRangeKey _ = False

    isInvalidRange (Condition.Range _ mi ma) = mi < 0 || mi > ma
    isInvalidRange _ = False

-- | Convert Post Policy to a string (e.g. for printing).
encode :: POSTPolicy -> B.ByteString
encode = BL.toStrict . JSON.encode

iso8601TimeFormat :: UTCTime -> [Char]
iso8601TimeFormat = iso8601Show
