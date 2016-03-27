{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Storage.SQLite.Internal
  ( ContentRow(ContentRow)
  , rowToContent
  , crId
  ) where

import           Data.Time.Clock                      (UTCTime)
import           Database.SQLite.Simple               (field)
import           Database.SQLite.Simple.FromRow       (FromRow, fromRow)

import           ZoomHub.Types.Internal.Content       (Content (Content),
                                                       contentActiveAt,
                                                       contentCompletedAt,
                                                       contentDzi, contentId,
                                                       contentInitializedAt,
                                                       contentMime,
                                                       contentProgress,
                                                       contentSize,
                                                       contentState, contentUrl)
import           ZoomHub.Types.Internal.ContentId     (ContentId)
import           ZoomHub.Types.Internal.ContentState  (ContentState)
import           ZoomHub.Types.Internal.ContentURI    (ContentURI)
import           ZoomHub.Types.Internal.DeepZoomImage (DeepZoomImage (DeepZoomImage),
                                                       dziHeight, dziTileFormat,
                                                       dziTileOverlap,
                                                       dziTileSize, dziWidth)

data ContentRow = ContentRow
  { crId             :: Maybe Integer
  , crHashId         :: ContentId
  , crUrl            :: ContentURI
  , crState          :: ContentState
  , crInitializedAt  :: Maybe UTCTime
  , crActiveAt       :: Maybe UTCTime
  , crCompletedAt    :: Maybe UTCTime
  , crMime           :: Maybe String
  , crSize           :: Maybe Integer
  , crProgress       :: Float
  , crDziWidth       :: Maybe Integer
  , crDziHeight      :: Maybe Integer
  , crDziTileSize    :: Maybe Integer
  , crDziTileOverlap :: Maybe Integer
  , crDziTileFormat  :: Maybe String
  } deriving (Show)

instance FromRow ContentRow where
  fromRow = ContentRow <$>
    field <*> field <*> field <*> field <*> field <*> field <*> field <*>
    field <*> field <*> field <*> field <*> field <*> field <*> field <*>
    field

rowToContent :: ContentRow -> Content
rowToContent cr = Content
    { contentId = crHashId cr
    , contentUrl = crUrl cr
    , contentState = crState cr
    , contentInitializedAt = crInitializedAt cr
    , contentActiveAt = crActiveAt cr
    , contentCompletedAt = crCompletedAt cr
    , contentMime = crMime cr
    , contentSize = crSize cr
    , contentProgress = crProgress cr
    , contentDzi = maybeDZI
    }
  where
    maybeDZIWidth = crDziWidth cr
    maybeDZIHeight = crDziHeight cr
    maybeDZITileSize = crDziTileSize cr
    maybeDZITileOverlap = crDziTileOverlap cr
    maybeDZITileFormat = crDziTileFormat cr
    maybeDZI =
      case (maybeDZIWidth, maybeDZIHeight, maybeDZITileSize,
            maybeDZITileOverlap, maybeDZITileFormat) of
      (Just dziWidth, Just dziHeight,
       Just dziTileSize, Just dziTileOverlap, Just dziTileFormat) ->
        Just DeepZoomImage{..}
      _ -> Nothing
