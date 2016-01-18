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

import           Data.Aeson                       (FromJSON, ToJSON,
                                                   genericParseJSON,
                                                   genericToJSON, parseJSON,
                                                   toJSON)
import           Data.Aeson.Casing                (aesonPrefix, camelCase)
import           GHC.Generics                     (Generic)

import           ZoomHub.Types.DeepZoomImage      (DeepZoomImage)
import qualified ZoomHub.Types.DeepZoomImage      as DZ
import qualified ZoomHub.Types.Internal.Content   as Internal
import           ZoomHub.Types.Internal.ContentId (ContentId, unId)


-- Content
data Content = Content
  { contentId        :: ContentId
  , contentUrl       :: String
  , contentReady     :: Bool
  , contentFailed    :: Bool
  , contentProgress  :: Float
  , contentShareUrl  :: String
  , contentEmbedHtml :: String
  , contentDzi       :: Maybe DeepZoomImage
  } deriving (Eq, Show, Generic)

instance ToJSON Content where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON Content where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

-- Constructor
fromInternal :: Internal.Content -> Content
fromInternal c = Content
  { contentId = cId
  , contentUrl = Internal.contentUrl c
  , contentReady = Internal.contentReady c
  , contentFailed = Internal.contentFailed c
  , contentProgress = Internal.contentProgress c
  , contentShareUrl = shareURL
  , contentEmbedHtml = embedHtml
  , contentDzi = dzi
  }
  where
    cId = Internal.contentId c
    -- TODO: Make hostname dynamic:
    shareURL = "http://zoom.it/" ++ unId cId
    embedHtml =
      "<script src=\"" ++ shareURL ++ ".js?width=auto&height=400px\"></script>"
    dzi = DZ.fromInternal cId <$> Internal.contentDzi c
