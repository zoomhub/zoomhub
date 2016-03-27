{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.SQLite
  ( getById
  , getByURL
  ) where

import           Database.SQLite.Simple          (Connection, Only (Only),
                                                  Query, query)

import           ZoomHub.Storage.SQLite.Internal (rowToContent)
import           ZoomHub.Types.Content           (Content)
import           ZoomHub.Types.ContentId         (ContentId, unId)
import           ZoomHub.Types.ContentURI        (ContentURI)


-- Public URL
getById :: Connection -> ContentId -> IO (Maybe Content)
getById conn cId =
  get conn "SELECT id, hashId, url, state, initializedAt,\
    \ activeAt, completedAt, mime, size,progress, dzi_width, dzi_height,\
    \ dzi_tileSize, dzi_tileOverlap, dzi_tileFormat FROM content\
    \ WHERE hashId = ?" (unId cId)

getByURL :: Connection -> ContentURI -> IO (Maybe Content)
getByURL conn uri =
    get conn "SELECT id, hashId, url, state, initializedAt, activeAt,\
      \ completedAt, mime, size,progress, dzi_width, dzi_height, dzi_tileSize,\
      \ dzi_tileOverlap, dzi_tileFormat FROM content WHERE url = ?" (show uri)

get :: Connection -> Query -> String -> IO (Maybe Content)
get conn q param = do
  results <- query conn q (Only param)
  case results of
    (r:_) -> return . Just . rowToContent $ r
    _     -> return Nothing
