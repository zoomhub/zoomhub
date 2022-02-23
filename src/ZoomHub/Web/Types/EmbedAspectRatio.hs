{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.EmbedAspectRatio
  ( EmbedAspectRatio (..),
    toCSSValue,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

data EmbedAspectRatio
  = Auto
  | Ratio Integer Integer
  deriving (Eq, Show)

toCSSValue :: EmbedAspectRatio -> Text
toCSSValue Auto = "auto"
toCSSValue (Ratio width height) = T.pack $ show width <> " / " <> show height
