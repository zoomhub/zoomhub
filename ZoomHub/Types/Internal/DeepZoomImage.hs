{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.Internal.DeepZoomImage
  ( DeepZoomImage(DeepZoomImage)
  , dziWidth
  , dziHeight
  , dziTileSize
  , dziTileOverlap
  , dziTileFormat
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as AC
import qualified GHC.Generics as GHC


data DeepZoomImage = DeepZoomImage
  { dziWidth :: Integer
  , dziHeight :: Integer
  , dziTileSize :: Integer
  , dziTileOverlap :: Integer
  , dziTileFormat :: String
  } deriving (Eq, Show, GHC.Generic)

instance Aeson.ToJSON DeepZoomImage where
   toJSON = Aeson.genericToJSON $ AC.aesonPrefix AC.camelCase
instance Aeson.FromJSON DeepZoomImage where
   parseJSON = Aeson.genericParseJSON $ AC.aesonPrefix AC.camelCase
