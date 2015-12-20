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
import Data.Aeson.Casing

import qualified Data.Text as T
import qualified Data.Time.Clock as DTC
import qualified GHC.Generics as GHC
import qualified Servant as S
import qualified ZoomHub.Types.Internal.Content as IC
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
   toJSON = genericToJSON $ aesonPrefix camelCase
instance Aeson.FromJSON Content where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

-- Constructor
fromInternal :: IC.Content -> Content
fromInternal c = Content
  { contentId = cid
  , contentUrl = IC.contentUrl c
  , contentReady = IC.contentReady c
  , contentFailed = IC.contentFailed c
  , contentProgress = IC.contentProgress c
  , contentShareUrl = shareURL
  , contentEmbedHtml = embedHtml
  , contentDzi = dzi
  }
  where
    cid = IC.contentId c
    shareURL = "http://zoom.it/" ++ (show cid)
    embedHtml = "<script src=\"" ++ shareURL ++ ".js\
      \?width=auto&height=400px\"></script>"
    dzi = case IC.contentDzi c of
            Nothing -> Nothing
            Just d  -> Just $ DZ.fromInternal cid d
