{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.Types.Content
  ( Content,
    ContentShareURI,
    contentDzi,
    contentFailed,
    contentId,
    contentProgress,
    contentReady,
    contentUrl,
    fromInternal,
    unContentShareURI,
  )
where

import Control.Monad (join)
import Data.Aeson (ToJSON, Value (String), genericToJSON, toJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.URI (URI, parseRelativeReference, relativeTo)
import ZoomHub.API.Types.DeepZoomImage (DeepZoomImage)
import qualified ZoomHub.API.Types.DeepZoomImage as DZ
import ZoomHub.Types.BaseURI (BaseURI, unBaseURI)
import qualified ZoomHub.Types.Content.Internal as Internal
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import ZoomHub.Types.ContentId (ContentId, unContentId)
import ZoomHub.Types.ContentState
  ( ContentState (CompletedFailure, CompletedSuccess),
  )
import ZoomHub.Types.ContentURI (ContentURI)

-- Content
data Content = Content
  { contentId :: ContentId,
    contentUrl :: ContentURI,
    contentReady :: Bool,
    contentFailed :: Bool,
    contentVerified :: Bool,
    contentProgress :: Double,
    contentShareUrl :: ContentShareURI,
    contentEmbedHtml :: String,
    contentDzi :: Maybe DeepZoomImage
  }
  deriving (Eq, Show, Generic)

-- Constructor
fromInternal :: BaseURI -> ContentBaseURI -> Internal.Content -> Content
fromInternal baseURI contentBaseURI c =
  Content
    { contentId = cId,
      contentUrl = Internal.contentURL c,
      contentReady = Internal.contentState c == CompletedSuccess,
      contentFailed = Internal.contentState c == CompletedFailure,
      contentVerified = isJust $ Internal.contentVerifiedAt c,
      contentProgress = Internal.contentProgress c,
      contentShareUrl = shareURI,
      contentEmbedHtml = embedHTML,
      contentDzi = join dzi
    }
  where
    cId = Internal.contentId c
    shareURI = ContentShareURI $ sharePathURI `relativeTo` unBaseURI baseURI
    sharePathURI = fromJust . parseRelativeReference $ unContentId cId
    scriptSource = show shareURI ++ ".js?width=auto&height=400px"
    embedHTML = "<script src=\"" ++ scriptSource ++ "\"></script>"
    dzi = DZ.fromInternal contentBaseURI cId <$> Internal.contentDZI c

-- JSON
instance ToJSON Content where
  toJSON = genericToJSON $ aesonPrefix camelCase

-- Types
newtype ContentShareURI = ContentShareURI {unContentShareURI :: URI}
  deriving (Eq)

instance Show ContentShareURI where
  show = show . unContentShareURI

instance ToJSON ContentShareURI where
  toJSON = String . T.pack . show . unContentShareURI
