{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Embed
  ( Embed
  , fromString
  ) where

import           Prelude                          hiding (fromInteger)

import qualified Data.ByteString.Char8            as BC
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Lucid                            (ToHtml, toHtml, toHtmlRaw)
import           Network.HTTP.Types               (decodePath)
import           Servant                          (FromText, fromText)
import           System.FilePath.Posix            (splitExtension)

import           ZoomHub.Types.Internal.ContentId (ContentId)
import qualified ZoomHub.Types.Internal.ContentId as ContentId

data EmbedDimension = Auto | Pixels Integer
  deriving (Eq, Generic, Show)

data Embed = Embed
  { embedContentId :: ContentId
  , embedWidth     :: Maybe EmbedDimension
  , embedHeight    :: Maybe EmbedDimension
  } deriving (Eq, Generic, Show)

fromString :: String -> Maybe Embed
fromString s = case maybeContentId of
    Nothing -> Nothing
    Just embedContentId ->
      Just Embed { embedContentId, embedWidth, embedHeight }
  where
    embedWidth = Nothing  -- TODO
    embedHeight = Nothing -- TODO
    pathAndQuery = decodePath (BC.pack s)
    pathSegments = fst pathAndQuery
    maybeContentId = case pathSegments of
      (p:[]) ->
        let idParts = splitExtension . T.unpack $ p in
        case idParts of
          (cId, ".js") -> Just $ ContentId.fromString cId
          _ -> Nothing
      _ -> Nothing

-- Text
instance FromText Embed where
  fromText t = fromString s
    where s = T.unpack t

-- HTML
instance ToHtml Embed where
  toHtml embed = toHtml $ show embed
  toHtmlRaw = toHtml
