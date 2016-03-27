{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Storage.SQLite
  ( getById
  , getByURL
  , crId
  ) where

import           Data.Monoid                    ((<>))
import           Data.String                    (IsString (fromString))
import           Data.Time.Clock                (UTCTime)
import           Database.SQLite.Simple         (Connection, Only (Only), Query,
                                                 field, query)
import           Database.SQLite.Simple.FromRow (FromRow, fromRow)

import           ZoomHub.Types.Content          (Content (Content),
                                                 contentActiveAt,
                                                 contentCompletedAt, contentDzi,
                                                 contentId,
                                                 contentInitializedAt,
                                                 contentMime, contentProgress,
                                                 contentSize, contentState,
                                                 contentUrl)
import           ZoomHub.Types.ContentId        (ContentId, unId)
import           ZoomHub.Types.ContentState     (ContentState)
import           ZoomHub.Types.ContentURI       (ContentURI)
import           ZoomHub.Types.DeepZoomImage    (DeepZoomImage (DeepZoomImage),
                                                 TileFormat, TileOverlap,
                                                 TileSize, dziHeight,
                                                 dziTileFormat, dziTileOverlap,
                                                 dziTileSize, dziWidth)



-- Public URL
getById :: Connection -> ContentId -> IO (Maybe Content)
getById conn cId =
  get conn (getQuery "hashId") (unId cId)

getByURL :: Connection -> ContentURI -> IO (Maybe Content)
getByURL conn uri =
  get conn (getQuery "url") (show uri)

get :: Connection -> Query -> String -> IO (Maybe Content)
get conn q param = do
  results <- query conn q (Only param)
  case results of
    (r:_) -> return . Just . rowToContent $ r
    _     -> return Nothing

-- Internal
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
  , crDziTileSize    :: Maybe TileSize
  , crDziTileOverlap :: Maybe TileOverlap
  , crDziTileFormat  :: Maybe TileFormat
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

getQuery :: String -> Query
getQuery fieldName = "SELECT id, hashId, url, state, initializedAt,\
    \ activeAt, completedAt, mime, size,progress, dzi_width, dzi_height,\
    \ dzi_tileSize, dzi_tileOverlap, dzi_tileFormat FROM content\
    \ WHERE " <> fromString fieldName <> " = ?"
