{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.API.Types.Config
  ( Config (..),
  )
where

import Data.Aeson (ToJSON, genericToJSON, toJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import GHC.Generics (Generic)

data Config = Config
  { configUploadsEnabled :: Bool
  , configUploadsMaxSizeMegabytes :: Integer
  }
  deriving (Eq, Show, Generic)

-- JSON
instance ToJSON Config where
  toJSON = genericToJSON $ aesonPrefix camelCase
