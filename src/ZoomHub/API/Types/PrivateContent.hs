{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.API.Types.PrivateContent
  ( PrivateContent,
    contentId,
    contentUrl,
    contentReady,
    contentFailed,
    contentVerified,
    contentProgress,
    contentDzi,
    fromInternal,
  )
where

import Prelude hiding (id)
import Data.Aeson (ToJSON, genericToJSON, toJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import ZoomHub.API.Types.DeepZoomImage (DeepZoomImage)
import qualified ZoomHub.API.Types.DeepZoomImage as DZ
import qualified ZoomHub.Types.Content.Internal as Internal
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentState
  ( ContentState (CompletedFailure, CompletedSuccess),
  )
import ZoomHub.Types.ContentURI (ContentURI)

-- Content
data PrivateContent = PrivateContent
  { contentId :: ContentId,
    contentUrl :: ContentURI,
    contentReady :: Bool,
    contentFailed :: Bool,
    contentVerified :: Bool,
    contentProgress :: Double,
    contentDzi :: Maybe DeepZoomImage
  }
  deriving (Eq, Show, Generic)

-- Constructor
fromInternal :: ContentBaseURI -> Internal.Content -> PrivateContent
fromInternal contentBaseURI c =
  PrivateContent
    { contentId = cId,
      contentUrl = Internal.contentURL c,
      contentReady = Internal.contentState c == CompletedSuccess,
      contentFailed = Internal.contentState c == CompletedFailure,
      contentVerified = isJust $ Internal.contentVerifiedAt c,
      contentProgress = Internal.contentProgress c,
      contentDzi = Internal.contentDZI c >>= DZ.fromInternal contentBaseURI cId
    }
  where
    cId = Internal.contentId c

-- JSON
instance ToJSON PrivateContent where
  toJSON = genericToJSON $ aesonPrefix camelCase
