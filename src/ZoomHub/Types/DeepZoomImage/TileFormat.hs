{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.DeepZoomImage.TileFormat
  ( TileFormat (..),
    fromString,
    fromText,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value (String), parseJSON, toJSON, withText)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Squeal.PostgreSQL (FromValue (..), PG, PGType (PGtext), ToParam (..))

data TileFormat = JPEG | PNG deriving (Eq)

fromString :: String -> Maybe TileFormat
fromString "jpg" = Just JPEG
fromString "jpeg" = Just JPEG
fromString "png" = Just PNG
fromString _ = Nothing

fromText :: T.Text -> Maybe TileFormat
fromText "jpg" = Just JPEG
fromText "jpeg" = Just JPEG
fromText "png" = Just PNG
fromText _ = Nothing

instance Show TileFormat where
  show JPEG = "jpg"
  show PNG = "png"

-- JSON
instance ToJSON TileFormat where
  toJSON = String . T.pack . show

instance FromJSON TileFormat where
  parseJSON = withText "TileFormat" $ \case
    "jpeg" -> pure JPEG
    "jpg" -> pure JPEG
    "png" -> pure PNG
    _ -> fail "invalid tile format"

-- Squeal / PostgreSQL
instance FromValue 'PGtext TileFormat where
  -- TODO: What if database value is not a valid?
  fromValue = fromJust . fromString <$> fromValue @'PGtext

type instance PG TileFormat = 'PGtext

instance ToParam TileFormat 'PGtext where
  toParam JPEG = toParam ("jpeg" :: Text)
  toParam PNG = toParam ("png" :: Text)
