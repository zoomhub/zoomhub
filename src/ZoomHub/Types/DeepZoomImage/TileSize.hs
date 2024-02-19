{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.DeepZoomImage.TileSize
  ( TileSize (..),
    fromString,
    fromInteger,
  )
where

import Data.Aeson (ToJSON, Value (Number), toJSON)
import Data.Aeson.Types (FromJSON (parseJSON), withScientific)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Squeal.PostgreSQL (FromPG (..), Inline (..), IsPG (..), PG, PGType (PGint4), ToPG (..))
import Prelude hiding (fromInteger)

data TileSize
  = TileSize254
  | TileSize256
  | TileSize510
  | TileSize1024
  deriving (Bounded, Enum, Eq)

fromString :: String -> Maybe TileSize
fromString "254" = Just TileSize254
fromString "256" = Just TileSize256
fromString "510" = Just TileSize510
fromString "1024" = Just TileSize1024
fromString _ = Nothing

fromInteger :: Integer -> Maybe TileSize
fromInteger 254 = Just TileSize254
fromInteger 256 = Just TileSize256
fromInteger 510 = Just TileSize510
fromInteger 1024 = Just TileSize1024
fromInteger _ = Nothing

toInt32 :: TileSize -> Int32
toInt32 TileSize254 = 254
toInt32 TileSize256 = 256
toInt32 TileSize510 = 510
toInt32 TileSize1024 = 1024

instance Show TileSize where
  show TileSize254 = "254"
  show TileSize256 = "256"
  show TileSize510 = "510"
  show TileSize1024 = "1024"

-- JSON
instance ToJSON TileSize where
  toJSON TileSize254 = Number 254
  toJSON TileSize256 = Number 256
  toJSON TileSize510 = Number 510
  toJSON TileSize1024 = Number 1024

instance FromJSON TileSize where
  parseJSON = withScientific "TileSize" $ \case
    254 -> pure TileSize254
    256 -> pure TileSize256
    510 -> pure TileSize510
    1024 -> pure TileSize1024
    _ -> fail "invalid tile size"

-- PostgreSQL / Squeal
instance IsPG TileSize where
  type PG TileSize = 'PGint4

instance FromPG TileSize where
  fromPG = fromJust . fromInteger . fromIntegral <$> fromPG @Int32

instance ToPG db TileSize where
  toPG = toPG . toInt32

instance Inline TileSize where
  inline = inline . toInt32

-- type instance PG TileSize = 'PGint4
