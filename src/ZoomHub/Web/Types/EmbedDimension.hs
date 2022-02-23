{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.EmbedDimension
  ( EmbedDimension (..),
    toCSSValue,
    parseCSSValue,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import Safe (readEitherSafe)
import Servant (FromHttpApiData, parseUrlPiece)
import ZoomHub.Utils (tshow)

-- Type
data EmbedDimension
  = Zero
  | Auto
  | Pixels Integer
  | Percentage Integer
  deriving (Eq, Show)

toCSSValue :: EmbedDimension -> Text
toCSSValue Auto = "auto"
toCSSValue Zero = "0"
toCSSValue (Pixels n) = tshow n <> "px"
toCSSValue (Percentage n) = tshow n <> "%"

parseCSSValue :: Text -> Either Text EmbedDimension
parseCSSValue "auto" = Right Auto
parseCSSValue "0" = Right Zero
parseCSSValue s = case unit of
  "px" -> readEitherSafe value & first T.pack >>= Right . Pixels
  "%" -> readEitherSafe value & first T.pack >>= Right . Percentage
  invalidUnit -> Left $ "Invalid CSS unit: " <> invalidUnit
  where
    value = T.unpack $ T.takeWhile isDigit s
    unit = T.dropWhile isDigit s

-- Servant
instance FromHttpApiData EmbedDimension where
  parseUrlPiece = parseCSSValue
