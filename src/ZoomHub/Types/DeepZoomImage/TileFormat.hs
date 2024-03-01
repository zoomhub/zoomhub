{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, ToJSON, Value (String), parseJSON, toJSON, withText)
import Data.Text (Text)
import qualified Data.Text as T
import Squeal.PostgreSQL (FromPG (..), Inline (..), IsPG (..), PG, PGType (PGtext), ToPG (..))

data TileFormat
  = JPEG
  | JPG
  | PNG
  deriving (Eq)

fromString :: String -> Maybe TileFormat
fromString = fromText . T.pack

fromText :: T.Text -> Maybe TileFormat
fromText "jpeg" = Just JPEG
fromText "jpg" = Just JPG
fromText "png" = Just PNG
fromText _ = Nothing

toText :: TileFormat -> T.Text
toText JPEG = "jpeg"
toText JPG = "jpg"
toText PNG = "png"

instance Show TileFormat where
  show JPEG = "jpeg"
  show JPG = "jpg"
  show PNG = "png"

-- JSON
instance ToJSON TileFormat where
  toJSON = String . T.pack . show

instance FromJSON TileFormat where
  parseJSON = withText "TileFormat" $ \case
    "jpeg" -> pure JPEG
    "jpg" -> pure JPG
    "png" -> pure PNG
    invalid -> fail $ "Invalid tile format: " <> T.unpack invalid

-- Squeal / PostgreSQL
instance IsPG TileFormat where
  type PG TileFormat = 'PGtext

instance FromPG TileFormat where
  fromPG = do
    value <- fromPG @Text
    case fromText value of
      Just format -> pure format
      Nothing -> throwError $ "Unknown tile format: \"" <> value <> "\""

instance ToPG db TileFormat where
  toPG = toPG . toText

instance Inline TileFormat where
  inline = inline . toText
