{-# LANGUAGE DataKinds #-}
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
import Squeal.PostgreSQL (FromValue (..), PG, PGType (PGint4), ToParam (..))
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
type instance PG TileSize = 'PGint4

instance ToParam TileSize 'PGint4 where
  toParam TileSize254 = toParam (254 :: Int32)
  toParam TileSize256 = toParam (256 :: Int32)
  toParam TileSize510 = toParam (510 :: Int32)
  toParam TileSize1024 = toParam (1024 :: Int32)

instance FromValue 'PGint4 TileSize where
  -- TODO: What if database value is not a valid?
  fromValue = convert <$> fromValue @'PGint4
    where
      convert :: Int32 -> TileSize
      convert = fromJust . fromInteger . fromIntegral
