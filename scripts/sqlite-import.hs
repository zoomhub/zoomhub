{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Data.Monoid                          ((<>))
import           Data.Time.Clock                      (UTCTime)
import           Database.SQLite.Simple               (Query, close,
                                                       execute, execute_, field,
                                                       open, query_)
import           Database.SQLite.Simple.FromRow       (FromRow, fromRow)
import           Database.SQLite.Simple.ToField       (toField)
import           Database.SQLite.Simple.ToRow         (ToRow, toRow)
import           System.Directory                     (getCurrentDirectory)
import           System.FilePath.Posix                ((</>))

import           ZoomHub.Types.Internal.Content       (Content,
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
                                                       contentSize, contentUrl,
                                                       fromURL)
import           ZoomHub.Types.Internal.ContentId     (fromString, unId)
import           ZoomHub.Types.Internal.DeepZoomImage (dziHeight, dziTileFormat,
                                                       dziTileOverlap,
                                                       dziTileSize, dziWidth)

data ContentRow = ContentRow
  { crId             :: Integer
  , crHashId         :: String
  , crUrl            :: String
  , crReady          :: Bool
  , crFailed         :: Bool
  , crProgress       :: Float
  , crMime           :: Maybe String
  , crSize           :: Maybe Integer
  , crActive         :: Maybe Bool
  , crActiveAt       :: Maybe UTCTime
  , crFinishedAt     :: Maybe UTCTime
  , crRawPath        :: Maybe String
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
    field <*> field <*> field

instance ToRow ContentRow where
  toRow (ContentRow{..}) =
    [ toField crId
    , toField crHashId
    , toField crUrl
    , toField crReady
    , toField crFailed
    , toField crProgress
    , toField crMime
    , toField crSize
    , toField crActive
    , toField crActiveAt
    , toField crFinishedAt
    , toField crRawPath
    , toField crDziWidth
    , toField crDziHeight
    , toField crDziTileSize
    , toField crDziTileOverlap
    , toField crDziTileFormat
    ]

contentToRow :: Content -> ContentRow
contentToRow c = ContentRow
    { crId = 0
    , crHashId = unId $ contentId c
    , crUrl = contentUrl c
    , crReady = contentReady c
    , crFailed = contentFailed c
    , crProgress = contentProgress c
    , crMime = contentMime c
    , crSize = contentSize c
    , crActive = contentActive c
    , crActiveAt = contentActiveAt c
    , crFinishedAt = contentFinishedAt c
    , crRawPath = contentRawPath c
    , crDziWidth = dziWidth <$> dzi
    , crDziHeight = dziHeight <$> dzi
    , crDziTileSize = dziTileSize <$> dzi
    , crDziTileOverlap = dziTileOverlap <$> dzi
    , crDziTileFormat = dziTileFormat <$> dzi
    }
  where dzi = contentDzi c

deleteContentTableQuery :: Query
deleteContentTableQuery = "DROP TABLE IF EXISTS content"

createContentTableQuery :: Query
createContentTableQuery =
  "CREATE TABLE IF NOT EXISTS content (" <>
    "id INTEGER PRIMARY KEY," <>
    "hashId TEXT UNIQUE NOT NULL," <>
    "url TEXT NOT NULL," <>
    "ready BOOLEAN DEFAULT 0 NOT NULL," <>
    "failed BOOLEAN DEFAULT 0 NOT NULL," <>
    "progress REAL DEFAULT 0.0 NOT NULL," <>
    "mime TEXT," <>
    "size INTEGER," <>
    "active BOOLEAN," <>
    "activeAt DATETIME," <>
    "finishedAt DATETIME," <>
    "rawPath TEXT," <>
    "dzi_width INTEGER," <>
    "dzi_height INTEGER," <>
    "dzi_tileSize INTEGER," <>
    "dzi_tileOverlap INTEGER," <>
    "dzi_tileFormat TEXT" <>
  ")"

main :: IO ()
main = do
  cd <- getCurrentDirectory
  conn <- open (cd </> "data" </> "content.db")

  execute_ conn deleteContentTableQuery
  execute_ conn createContentTableQuery

  let sample = contentToRow (fromURL (fromString "abc") "http://example.com")
  execute conn "INSERT INTO content (id, hashId, url, ready, failed, progress, \
    \ mime, size, active, activeAt, finishedAt, rawPath, dzi_width, \
    \ dzi_height, dzi_tileSize, dzi_tileOverlap, dzi_tileFormat) \
    \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)" sample
  r <- query_ conn "SELECT * FROM content" :: IO [ContentRow]
  mapM_ print r

  close conn
