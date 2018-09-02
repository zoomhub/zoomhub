{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.EmbedId
  ( EmbedId
  , unEmbedId
  ) where

import           Data.Bifunctor          (first)
import qualified Data.ByteString.Char8   as BC
import qualified Data.Text               as T
import           Network.HTTP.Types      (decodePath)
import           Servant                 (FromHttpApiData, parseUrlPiece)
import           System.FilePath         (splitExtension)

import           ZoomHub.Types.ContentId (ContentId)
import qualified ZoomHub.Types.ContentId as ContentId

newtype EmbedId = EmbedId
  { unEmbedId :: ContentId
  } deriving (Eq, Show)

fromString :: String -> Either String EmbedId
fromString s =
  case maybeContentId of
    Just contentId -> Right $ EmbedId contentId
    _              -> Left "Invalid embed ID"
  where
    pathAndQuery = decodePath (BC.pack s)
    (pathSegments, _) = pathAndQuery
    maybeContentId =
      case pathSegments of
        [p] ->
          let idParts = splitExtension . T.unpack $ p
           in case idParts of
                (cId, ".js") -> Just $ ContentId.fromString cId
                _            -> Nothing
        _ -> Nothing

-- Text
instance FromHttpApiData EmbedId where
  parseUrlPiece p = first T.pack $ fromString . T.unpack $ p
