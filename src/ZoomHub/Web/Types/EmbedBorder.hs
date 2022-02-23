{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.EmbedBorder
  ( EmbedBorder (..),
    toCSSValue,
    parseCSSValue,
  )
where

import Data.Text (Text)
import Servant (FromHttpApiData, parseUrlPiece)

-- Type
data EmbedBorder
  = None
  | Default
  deriving (Eq, Show)

toCSSValue :: EmbedBorder -> Text
toCSSValue None = "none"
toCSSValue Default = "1px solid black"

-- NOTE: We don’t allow the default border to be passed in as query param:
parseCSSValue :: Text -> Either Text EmbedBorder
parseCSSValue "none" = Right None
parseCSSValue value = Left $ "Invalid value: " <> value

-- Text
instance FromHttpApiData EmbedBorder where
  parseUrlPiece = parseCSSValue
