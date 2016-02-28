{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.EmbedParam
  ( EmbedParam
  , fromString
  , embedParamContentId
  , embedParamWidth
  , embedParamHeight
  , embedParamId
  ) where

import qualified Data.ByteString.Char8            as BC
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Network.HTTP.Types               (decodePath)
import           Servant                          (FromText, fromText)
import           System.FilePath.Posix            (splitExtension)

import           ZoomHub.Types.EmbedDimension     (EmbedDimension (..),
                                                   parseCSSValue)
import           ZoomHub.Types.Internal.ContentId (ContentId)
import qualified ZoomHub.Types.Internal.ContentId as ContentId

data EmbedParam = EmbedParam
  { embedParamContentId :: ContentId
  , embedParamId        :: Maybe String
  , embedParamWidth     :: Maybe EmbedDimension
  , embedParamHeight    :: Maybe EmbedDimension
  } deriving (Eq, Generic, Show)

fromString :: String -> Maybe EmbedParam
fromString s = case maybeContentId of
    Just embedParamContentId ->
      Just EmbedParam
        { embedParamContentId
        , embedParamHeight
        , embedParamId
        , embedParamWidth
        }
    _ -> Nothing
  where
    toDimension :: String -> Maybe EmbedDimension
    toDimension name = maybe Nothing parseCSSValue (getQueryParam name)

    getQueryParam :: String -> Maybe String
    getQueryParam name = case lookup (BC.pack name) queryParams of
      Just (Just bs) -> Just $ BC.unpack bs
      _ -> Nothing

    embedParamId = getQueryParam "id"
    embedParamWidth = toDimension "width"
    embedParamHeight = toDimension "height"
    pathAndQuery = decodePath (BC.pack s)
    (pathSegments, queryParams) = pathAndQuery
    maybeContentId = case pathSegments of
      [p] ->
        let idParts = splitExtension . T.unpack $ p in
        case idParts of
          (cId, ".js") -> Just $ ContentId.fromString cId
          _ -> Nothing
      _ -> Nothing

-- Text
instance FromText EmbedParam where
  fromText t = fromString s
    where s = T.unpack t
