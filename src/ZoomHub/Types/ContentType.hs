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
  ( Inline (inline),
    IsPG,
    PG,
    PGType (PGint4),
    FromPG(fromPG)
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

instance IsPG ContentType where
  type PG ContentType = 'PGint4

instance Inline ContentType where
  inline = inline . toPGint4

instance FromPG ContentType where
  fromPG = fromPGint4 <$> fromPG @Int32
