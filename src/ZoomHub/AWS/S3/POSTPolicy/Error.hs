{-# LANGUAGE DerivingStrategies #-}

-- Adapted from `minio-hs` under Apache License 2
module ZoomHub.AWS.S3.POSTPolicy.Error
  ( Error (..),
  )
where

-- | Possible validation errors when creating a `POSTPolicy`.
data Error
  = KeyNotSpecified
  | BucketNotSpecified
  | ConditionKeyEmpty
  | RangeInvalid
  deriving stock (Show, Eq)
