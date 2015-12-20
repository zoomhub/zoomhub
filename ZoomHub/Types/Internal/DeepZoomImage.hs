{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.Internal.DeepZoomImage where


import Data.Aeson as Aeson
import Data.Aeson.Casing

import qualified GHC.Generics as GHC


data DeepZoomImage = DeepZoomImage
  { dziWidth :: Integer
  , dziHeight :: Integer
  , dziTileSize :: Integer
  , dziTileOverlap :: Integer
  , dziTileFormat :: String
  } deriving (Eq, Show, GHC.Generic)

instance Aeson.ToJSON DeepZoomImage where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance Aeson.FromJSON DeepZoomImage where
   parseJSON = genericParseJSON $ aesonPrefix camelCase
