{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- Adapted from `minio-hs` under Apache License 2
module ZoomHub.AWS.S3.POSTPolicy.Condition
  ( Condition (..),
    bucket,
    contentLengthRange,
    contentType,
    key,
    keyStartsWith,
    successActionStatus,
  )
where

import Data.Aeson (ToJSON (toEncoding, toJSON), foldable, object, pairs, (.=))
import qualified Data.Aeson.Key as Key
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

data Condition
  = StartsWith Text Text
  | Equals Text Text
  | Range Text Int64 Int64
  deriving stock (Show, Eq)

instance ToJSON Condition where
  toJSON (StartsWith k v) = toJSON ["starts-with", k, v]
  toJSON (Equals k v) = object [(Key.fromText k) .= v]
  toJSON (Range k minVal maxVal) =
    toJSON [toJSON k, toJSON minVal, toJSON maxVal]

  toEncoding (StartsWith k v) = foldable ["starts-with", k, v]
  toEncoding (Equals k v) = pairs ((Key.fromText k) .= v)
  toEncoding (Range k minVal maxVal) =
    foldable [toJSON k, toJSON minVal, toJSON maxVal]

---

-- |
-- Represents a bucket in the object store
type Bucket = Text

-- |
-- Represents an object name
type Object = Text

-- | Set the bucket name that the upload should use.
bucket :: Bucket -> Condition
bucket = Equals "bucket"

-- | Set the content length range constraint with minimum and maximum
-- byte count values.
contentLengthRange :: Int64 -> Int64 -> Condition
contentLengthRange = Range "content-length-range"

-- | Set the `Content-Type` header for the upload.
contentType :: Text -> Condition
contentType = Equals "Content-Type"

-- | Set the object name constraint for the upload.
key :: Object -> Condition
key = Equals "key"

-- | Set the object name prefix constraint for the upload.
keyStartsWith :: Object -> Condition
keyStartsWith = StartsWith "key"

-- | Status code that the S3-server should send on a successful POST
-- upload
successActionStatus :: Int -> Condition
successActionStatus n = Equals "success_action_status" (T.pack $ show n)
