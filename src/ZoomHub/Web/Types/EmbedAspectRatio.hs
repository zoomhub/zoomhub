module ZoomHub.Web.Types.EmbedAspectRatio
  ( EmbedAspectRatio (..),
    toCSSValue,
  )
where

data EmbedAspectRatio
  = Auto
  | Ratio Integer Integer
  deriving (Eq, Show)

toCSSValue :: EmbedAspectRatio -> String
toCSSValue Auto = "auto"
toCSSValue (Ratio width height) = show width <> " / " <> show height
