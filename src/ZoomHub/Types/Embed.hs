{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Types.Embed
  ( Embed
  , EmbedDimension(..)
  , mkEmbed
  , embedContentId
  , embedWidth
  , embedHeight
  , embedBody
  , embedDZI
  ) where

import           Prelude                              hiding (fromInteger)

-- import qualified Data.ByteString.Char8            as BC
-- import qualified Data.Text                        as T
import           GHC.Generics                         (Generic)
import           Lucid                                (ToHtml, toHtml,
                                                       toHtmlRaw)

import           ZoomHub.Types.Internal.ContentId     (ContentId)
import           ZoomHub.Types.Internal.DeepZoomImage

data EmbedDimension = Auto | Pixels Integer
  deriving (Eq, Generic, Show)

data Embed = Embed
  { embedContentId :: ContentId
  , embedBody      :: String
  , embedDZI       :: DeepZoomImage
  , embedWidth     :: Maybe EmbedDimension
  , embedHeight    :: Maybe EmbedDimension
  } deriving (Eq, Generic, Show)

mkEmbed :: ContentId ->
           String ->
           DeepZoomImage ->
           Maybe EmbedDimension ->
           Maybe EmbedDimension ->
           Embed
mkEmbed embedContentId embedBody embedDZI embedWidth embedHeight = Embed{..}

-- HTML
instance ToHtml Embed where
  toHtml embed = toHtml . embedBody $ embed
  toHtmlRaw = toHtml
