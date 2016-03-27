{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.SQLite
  ( getById
  , getByURL
  ) where

import           Database.SQLite.Simple            (Only (Only), Query, close,
                                                    open, query)

import           ZoomHub.Storage.SQLite.Internal   (rowToContent)
import           ZoomHub.Types.DatabasePath        (DatabasePath,
                                                    unDatabasePath)
import           ZoomHub.Types.Internal.Content    (Content)
import           ZoomHub.Types.Internal.ContentId  (ContentId, unId)
import           ZoomHub.Types.Internal.ContentURI (ContentURI)


-- Public URL
getById :: DatabasePath -> ContentId -> IO (Maybe Content)
getById dbPath cId =
  get dbPath "SELECT id, hashId, url, state, initializedAt,\
    \ activeAt, completedAt, mime, size,progress, dzi_width, dzi_height,\
    \ dzi_tileSize, dzi_tileOverlap, dzi_tileFormat FROM content\
    \ WHERE hashId = ?" (unId cId)

getByURL :: DatabasePath -> ContentURI -> IO (Maybe Content)
getByURL dbPath uri =
    get dbPath "SELECT id, hashId, url, state, initializedAt, activeAt,\
      \ completedAt, mime, size,progress, dzi_width, dzi_height, dzi_tileSize,\
      \ dzi_tileOverlap, dzi_tileFormat FROM content WHERE url = ?" (show uri)

get :: DatabasePath -> Query -> String -> IO (Maybe Content)
get dbPath q param = do
  conn    <- open (unDatabasePath dbPath)
  results <- query conn q (Only param)
  close conn
  case results of
    (r:_) -> return . Just . rowToContent $ r
    _     -> return Nothing
