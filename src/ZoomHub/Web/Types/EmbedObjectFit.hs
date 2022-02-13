{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.EmbedObjectFit
  ( EmbedObjectFit (..),
  )
where

import Servant (FromHttpApiData, parseUrlPiece)
import ZoomHub.Web.Types.OpenSeadragonViewerConfig (ObjectFit (..))

-- Type
newtype EmbedObjectFit = EmbedObjectFit {unEmbedObjectFit :: ObjectFit}
  deriving (Eq, Show)

-- Text
instance FromHttpApiData EmbedObjectFit where
  parseUrlPiece "contain" = Right $ EmbedObjectFit Contain
  parseUrlPiece "cover" = Right $ EmbedObjectFit Cover
  parseUrlPiece _ = Left "Invalid object fit"
