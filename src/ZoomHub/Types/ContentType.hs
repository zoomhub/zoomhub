{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.ContentType
  ( ContentType (..),
  )
where

import Data.Int (Int32)
import Squeal.PostgreSQL
  ( FromValue (..),
    Literal (..),
    PG,
    PGType (PGint4),
    ToParam (..),
  )

data ContentType
  = Unknown
  | Image
  | Webpage
  | FlickrImage
  | GigaPan
  | Zoomify
  | PDF10
  | PDF11
  | WebpageThumbnail
  deriving (Eq, Show)

-- Squeal / PostgreSQL
fromPGint4 :: Int32 -> ContentType
fromPGint4 0 = Unknown
fromPGint4 1 = Image
fromPGint4 2 = Webpage
fromPGint4 3 = FlickrImage
fromPGint4 6 = GigaPan
fromPGint4 7 = Zoomify
fromPGint4 10 = PDF10
fromPGint4 11 = PDF11
fromPGint4 14 = WebpageThumbnail
fromPGint4 t = error $ "Invalid ContentType: " <> show t

toPGint4 :: ContentType -> Int32
toPGint4 Unknown = 0
toPGint4 Image = 1
toPGint4 Webpage = 2
toPGint4 FlickrImage = 3
toPGint4 GigaPan = 6
toPGint4 Zoomify = 7
toPGint4 PDF10 = 10
toPGint4 PDF11 = 11
toPGint4 WebpageThumbnail = 14

type instance PG ContentType = 'PGint4

instance ToParam ContentType 'PGint4 where
  toParam = toParam . toPGint4

instance FromValue 'PGint4 ContentType where
  -- TODO: What if database value is not a valid?
  fromValue = fromPGint4 <$> fromValue @'PGint4

instance Literal ContentType where
  literal = fromIntegral . toPGint4
