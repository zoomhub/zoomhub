{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Types.ContentBaseURI
  ( ContentBaseURI
  , unContentBaseURI
  , mkContentBaseURI
  )
where

import Data.Aeson ((.=), ToJSON, object, toJSON)
import Network.URI (URI, uriIsAbsolute)

newtype ContentBaseURI = ContentBaseURI { unContentBaseURI :: URI } deriving (Eq)

instance ToJSON ContentBaseURI where
  toJSON ContentBaseURI{..} =
    object
      [ "unContentBaseURI" .= show unContentBaseURI
      ]

mkContentBaseURI :: URI -> Maybe ContentBaseURI
mkContentBaseURI uri
  | uriIsAbsolute uri = pure $ ContentBaseURI uri
  | otherwise = Nothing
