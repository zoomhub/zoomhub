{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Types.ContentBaseURI
  ( ContentBaseURI,
    unContentBaseURI,
    mkContentBaseURI,
  )
where

import Data.Aeson (ToJSON, Value (String), toJSON)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import Network.URI (URI, uriIsAbsolute, uriPath)

newtype ContentBaseURI = ContentBaseURI {unContentBaseURI :: URI} deriving (Eq)

instance ToJSON ContentBaseURI where
  toJSON ContentBaseURI {..} = String . T.pack . show $ unContentBaseURI

mkContentBaseURI :: URI -> Maybe ContentBaseURI
mkContentBaseURI uri
  | uriIsAbsolute uri = pure $ ContentBaseURI $ ensureTrailingSlash uri
  | otherwise = Nothing
  where
    ensureTrailingSlash url =
      let path = uriPath url
       in if path == "" || "/" `isSuffixOf` path
            then url
            else url {uriPath = path <> "/"}
