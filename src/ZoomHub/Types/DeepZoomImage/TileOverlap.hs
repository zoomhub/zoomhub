{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.DeepZoomImage.TileOverlap
  ( TileOverlap (..),
    fromString,
    fromInteger,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (Number), toJSON, withScientific)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Squeal.PostgreSQL (FromPG (..), Inline (..), IsPG (..), PG, PGType (PGint4), ToPG (..))
import Prelude hiding (fromInteger, toInteger)

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

toInt32 :: TileOverlap -> Int32
toInt32 TileOverlap1 = 1
toInt32 TileOverlap0 = 0

-- Show
instance Show TileOverlap where
  show TileOverlap1 = "1"
  show TileOverlap0 = "0"

-- JSON
instance ToJSON TileOverlap where
  toJSON TileOverlap1 = Number 1
  toJSON TileOverlap0 = Number 0

instance FromJSON TileOverlap where
  parseJSON = withScientific "TileOverlap" $ \case
    1 -> pure TileOverlap1
    0 -> pure TileOverlap0
    _ -> fail "invalid tile overlap"

-- PostgreSQL / Squeal
instance IsPG TileOverlap where
  type PG TileOverlap = 'PGint4

instance FromPG TileOverlap where
  fromPG = fromJust . fromInteger . fromIntegral <$> fromPG @Int32

instance ToPG db TileOverlap where
  toPG = toPG . toInt32

instance Inline TileOverlap where
  inline = inline . toInt32
