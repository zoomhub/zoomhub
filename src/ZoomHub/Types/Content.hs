{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.Content
  ( Content
  , contentId
  , contentUrl
  , contentReady
  , contentFailed
  , contentProgress
  , contentDzi
  , fromInternal
  ) where


import Data.Aeson as Aeson

import qualified Data.Aeson.Casing as AC
import qualified GHC.Generics as GHC
import qualified ZoomHub.Types.Internal.Content as IC
import qualified ZoomHub.Types.Internal.ContentId as IC
import qualified ZoomHub.Types.DeepZoomImage as DZ

-- Content
data Content = Content
  { contentId :: IC.ContentId
  , contentUrl :: String
  , contentReady :: Bool
  , contentFailed :: Bool
  , contentProgress :: Float
  , contentShareUrl :: String
  , contentEmbedHtml :: String
  , contentDzi :: Maybe DZ.DeepZoomImage
  } deriving (Eq, Show, GHC.Generic)

instance Aeson.ToJSON Content where
   toJSON = Aeson.genericToJSON $ AC.aesonPrefix AC.camelCase
instance Aeson.FromJSON Content where
   parseJSON = Aeson.genericParseJSON $ AC.aesonPrefix AC.camelCase

-- Constructor
fromInternal :: IC.Content -> Content
fromInternal c = Content
  { contentId = cId
  , contentUrl = IC.contentUrl c
  , contentReady = IC.contentReady c
  , contentFailed = IC.contentFailed c
  , contentProgress = IC.contentProgress c
  , contentShareUrl = shareURL
  , contentEmbedHtml = embedHtml
  , contentDzi = dzi
  }
  where
    cId = IC.contentId c
    -- TODO: Make hostname dynamic:
    shareURL = "http://zoom.it/" ++ (show cId)
    embedHtml =
      "<script src=\"" ++ shareURL ++ ".js?width=auto&height=400px\"></script>"
    dzi = (DZ.fromInternal cId) <$> IC.contentDzi c