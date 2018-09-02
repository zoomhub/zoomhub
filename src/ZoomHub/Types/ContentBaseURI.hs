{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Types.ContentBaseURI
  ( ContentBaseURI
  , contentBaseHost
  , contentBasePath
  , mkContentBaseURI
  ) where

import           Data.Aeson  (ToJSON, object, toJSON, (.=))
import           Network.URI (URI, uriIsAbsolute, uriIsRelative)

data ContentBaseURI = ContentBaseURI
  { contentBaseHost :: URI
  , contentBasePath :: URI
  } deriving (Eq)

instance ToJSON ContentBaseURI where
  toJSON ContentBaseURI {..} =
    object ["host" .= show contentBaseHost, "path" .= show contentBasePath]

mkContentBaseURI :: URI -> URI -> Maybe ContentBaseURI
mkContentBaseURI base path =
  case (uriIsAbsolute base, uriIsRelative path) of
    (True, True) -> Just (ContentBaseURI base path)
    _            -> Nothing
