{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.API.Types.Config
  ( Config (..),
  )
where

import Data.Aeson (ToJSON, genericToJSON, toJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import GHC.Generics (Generic)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.Environment (Environment)

data Config = Config
  { configEnvironment :: Environment,
    configBaseURI :: BaseURI,
    configUploadsEnabled :: Bool,
    configUploadsMaxSizeMegabytes :: Integer
  }
  deriving (Generic)

-- JSON
instance ToJSON Config where
  toJSON = genericToJSON $ aesonPrefix camelCase
