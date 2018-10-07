{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module ZoomHub.Types.DeepZoomImage.TileSize
  ( TileSize(..)
  , fromString
  , fromInteger
  ) where

import           Prelude                          hiding (fromInteger)

import           Data.Aeson                       (ToJSON, Value (Number),
                                                   toJSON)
import           Data.Int                         (Int32)
import           Data.Maybe                       (fromJust)
import           Database.SQLite.Simple           (SQLData (SQLInteger))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           Squeal.PostgreSQL                (FromValue (..),
                                                   PGType (PGint4))

data TileSize = TileSize254 | TileSize256 | TileSize1024
  deriving (Bounded, Enum, Eq)

fromString :: String -> Maybe TileSize
fromString "254" = Just TileSize254
fromString "256" = Just TileSize256
fromString "1024" = Just TileSize1024
fromString _ = Nothing

fromInteger :: Integer -> Maybe TileSize
fromInteger 254 = Just TileSize254
fromInteger 256 = Just TileSize256
fromInteger 1024 = Just TileSize1024
fromInteger _ = Nothing

instance Show TileSize where
  show TileSize254 = "254"
  show TileSize256 = "256"
  show TileSize1024 = "1024"

-- Tile size: JSON
instance ToJSON TileSize where
  toJSON TileSize254 = Number 254
  toJSON TileSize256 = Number 256
  toJSON TileSize1024 = Number 1024

-- Tile size: SQLite
instance ToField TileSize where
  toField TileSize254 = SQLInteger 254
  toField TileSize256 = SQLInteger 256
  toField TileSize1024 = SQLInteger 1024

instance FromField TileSize where
  fromField (Field (SQLInteger 254) _) = Ok TileSize254
  fromField (Field (SQLInteger 256) _) = Ok TileSize256
  fromField (Field (SQLInteger 1024) _) = Ok TileSize1024
  fromField f =
    returnError ConversionFailed f "invalid Deep Zoom image tile size"

instance FromValue 'PGint4 TileSize where
  -- TODO: What if database value is not a valid?
  fromValue = convert <$> fromValue @'PGint4
    where
      convert :: Int32 -> TileSize
      convert = fromJust . fromInteger . fromIntegral
