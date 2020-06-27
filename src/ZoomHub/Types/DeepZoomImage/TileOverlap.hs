{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.DeepZoomImage.TileOverlap
  ( TileOverlap(..)
  , fromString
  , fromInteger
  ) where

import Prelude hiding (fromInteger)

import Data.Aeson (ToJSON, Value(Number), toJSON)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Squeal.PostgreSQL (FromValue(..), PG, PGType(PGint4), ToParam(..))

data TileOverlap = TileOverlap0 | TileOverlap1
  deriving (Bounded, Enum, Eq)

fromString :: String -> Maybe TileOverlap
fromString "1" = Just TileOverlap1
fromString "0" = Just TileOverlap0
fromString _ = Nothing

fromInteger :: Integer -> Maybe TileOverlap
fromInteger 1 = Just TileOverlap1
fromInteger 0 = Just TileOverlap0
fromInteger _ = Nothing

-- Show
instance Show TileOverlap where
  show TileOverlap1 = "1"
  show TileOverlap0 = "0"

-- JSON
instance ToJSON TileOverlap where
  toJSON TileOverlap1 = Number 1
  toJSON TileOverlap0 = Number 0

-- PostgreSQL / Squeal
type instance PG TileOverlap = 'PGint4
instance ToParam TileOverlap 'PGint4 where
  toParam TileOverlap1 = toParam (1 :: Int32)
  toParam TileOverlap0 = toParam (0 :: Int32)

instance FromValue 'PGint4 TileOverlap where
  -- TODO: What if database value is not a valid?
  fromValue = convert <$> fromValue @'PGint4
    where
      convert :: Int32 -> TileOverlap
      convert = fromJust . fromInteger . fromIntegral
