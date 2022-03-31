module ZoomHub.API.Types.Content
  ( Content (..),
    toPublic,
    fromInternal,
  )
where

import Data.Aeson (ToJSON, toJSON)
import ZoomHub.API.Types.PrivateContent (PrivateContent)
import ZoomHub.API.Types.PublicContent (PublicContent)
import qualified ZoomHub.API.Types.PublicContent as PublicContent
import ZoomHub.Types.BaseURI (BaseURI)
import qualified ZoomHub.Types.Content as Internal
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)

data Content
  = Public PublicContent
  | Private PrivateContent

-- JSON
instance ToJSON Content where
  toJSON (Public content) = toJSON content
  toJSON (Private content) = toJSON content

toPublic :: Content -> Maybe PublicContent
toPublic (Public content) = Just content
toPublic (Private _) = Nothing

fromInternal :: BaseURI -> ContentBaseURI -> Internal.Content -> Content
fromInternal baseURI contentBaseURI =
  -- TODO: Distinguish between private and public content:
  Public . PublicContent.fromInternal baseURI contentBaseURI
