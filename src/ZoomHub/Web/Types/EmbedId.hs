{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.EmbedId
  ( EmbedId
  , unEmbedId
  ) where

import qualified Data.ByteString.Char8   as BC
import qualified Data.Text               as T
import           Network.HTTP.Types      (decodePath)
import           Servant                 (FromText, fromText)
import           System.FilePath         (splitExtension)

import           ZoomHub.Types.ContentId (ContentId)
import qualified ZoomHub.Types.ContentId as ContentId

newtype EmbedId = EmbedId { unEmbedId :: ContentId } deriving (Eq, Show)

fromString :: String -> Maybe EmbedId
fromString s = case maybeContentId of
    Just contentId -> return $ EmbedId contentId
    _              -> Nothing
  where
    pathAndQuery = decodePath (BC.pack s)
    (pathSegments, _) = pathAndQuery
    maybeContentId = case pathSegments of
      [p] ->
        let idParts = splitExtension . T.unpack $ p in
        case idParts of
          (cId, ".js") -> Just $ ContentId.fromString cId
          _ -> Nothing
      _ -> Nothing

-- Text
instance FromText EmbedId where
  fromText = fromString . T.unpack
