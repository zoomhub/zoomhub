{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Storage.SQLite.Internal
  ( ContentRow(ContentRow)
  , rowToContent
  , contentToRow
  ) where

import           Data.Time.Clock                      (UTCTime)
import           Database.SQLite.Simple               (field)
import           Database.SQLite.Simple.FromRow       (FromRow, fromRow)

import           ZoomHub.Types.Internal.Content       (Content (Content),
                                                       contentActive,
                                                       contentActiveAt,
                                                       contentDzi,
                                                       contentFailed,
                                                       contentFinishedAt,
                                                       contentId, contentId,
                                                       contentMime,
                                                       contentProgress,
                                                       contentRawPath,
                                                       contentReady,
                                                       contentSize, contentUrl)
import           ZoomHub.Types.Internal.ContentId     (fromString, unId)
import           ZoomHub.Types.Internal.DeepZoomImage (DeepZoomImage (DeepZoomImage),
                                                       dziHeight, dziTileFormat,
                                                       dziTileOverlap,
                                                       dziTileSize, dziWidth)

data ContentRow = ContentRow
  { crId             :: Maybe Integer
  , crHashId         :: String
  , crUrl            :: String
  , crState          :: String
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
    { contentId = fromString (crHashId cr)
    , contentUrl = crUrl cr
    , contentReady = crState cr == "completed:success"
    , contentFailed = crState cr == "completed:failure"
    , contentProgress = crProgress cr
    , contentMime = crMime cr
    , contentSize = crSize cr
    , contentActive = Just (crState cr == "active")
    , contentActiveAt = crActiveAt cr
    , contentFinishedAt = crCompletedAt cr
    , contentRawPath = Nothing
    , contentDzi = maybeDZI
    }
  where
    mDziWidth = crDziWidth cr
    mDziHeight = crDziHeight cr
    mDziTileSize = crDziTileSize cr
    mDziTileOverlap = crDziTileOverlap cr
    mDziTileFormat = crDziTileFormat cr
    maybeDZI =
      case (mDziWidth, mDziHeight,
        mDziTileSize, mDziTileOverlap, mDziTileFormat) of
      (Just dziWidth, Just dziHeight,
       Just dziTileSize, Just dziTileOverlap, Just dziTileFormat) ->
        Just DeepZoomImage{..}
      _ -> Nothing

contentToRow :: Content -> ContentRow
contentToRow c = ContentRow
    { crId = Nothing
    , crHashId = unId $ contentId c
    , crUrl = contentUrl c
    , crState = flagsToState (contentReady c) (contentFailed c)
    , crInitializedAt = Nothing
    , crActiveAt = contentActiveAt c
    , crCompletedAt = contentFinishedAt c
    , crMime = contentMime c
    , crSize = contentSize c
    , crProgress = contentProgress c
    , crDziWidth = dziWidth <$> dzi
    , crDziHeight = dziHeight <$> dzi
    , crDziTileSize = dziTileSize <$> dzi
    , crDziTileOverlap = dziTileOverlap <$> dzi
    , crDziTileFormat = dziTileFormat <$> dzi
    }
  where
    dzi = contentDzi c

    flagsToState :: Bool -> Bool -> String
    flagsToState ready failed
      | not ready && not failed = "initialized"
      | ready && not failed     = "completed:success"
      | not ready && failed     = "completed:failure"
      | otherwise =
          error "ZoomHub.Storage.SQLite.Internal.flagsToState: Invalid state."
