module ZoomHub.Web.Types.EmbedBorder
  ( EmbedBorder (..),
    toCSSValue,
    parseCSSValue,
  )
where

import Data.Bifunctor (first)
import qualified Data.Text as T
import Servant (FromHttpApiData, parseUrlPiece)

-- Type
data EmbedBorder
  = None
  | Default
  deriving (Eq, Show)

toCSSValue :: EmbedBorder -> String
toCSSValue None = "none"
toCSSValue Default = "1px solid black"

-- NOTE: We don’t allow the default border to be passed in as query param:
parseCSSValue :: String -> Either String EmbedBorder
parseCSSValue "none" = Right None
parseCSSValue value = Left $ "Invalid value: " <> value

-- Text
instance FromHttpApiData EmbedBorder where
  parseUrlPiece p = first T.pack $ parseCSSValue . T.unpack $ p
