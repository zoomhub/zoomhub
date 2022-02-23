{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.EmbedBackground
  ( EmbedBackground (..),
    toCSSValue,
  )
where

import Data.Text (Text)
import Servant (FromHttpApiData, parseUrlPiece)

data EmbedBackground
  = None
  | Black
  | White
  deriving (Eq, Show)

parse :: Text -> Either Text EmbedBackground
parse value =
  case value of
    "none" -> Right None
    "black" -> Right Black
    "white" -> Right White
    s -> Left $ "Invalid value: " <> s

toCSSValue :: EmbedBackground -> Text
toCSSValue value =
  case value of
    None -> "none"
    Black -> "#000"
    White -> "#fff"

instance FromHttpApiData EmbedBackground where
  parseUrlPiece = parse
