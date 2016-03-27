{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Data.Monoid                          ((<>))
import           Data.Time.Clock                      (UTCTime)
import           Database.SQLite.Simple               (Query, close, execute,
                                                       execute_, field, open,
                                                       withTransaction)
import           Database.SQLite.Simple.FromRow       (FromRow, fromRow)
import           Database.SQLite.Simple.ToField       (toField)
import           Database.SQLite.Simple.ToRow         (ToRow, toRow)
import           System.Directory                     (getCurrentDirectory,
                                                       getDirectoryContents)
import           System.FilePath.Posix                (dropExtension,
                                                       takeExtension, (</>))

import           ZoomHub.Storage.File                 (getById)
import           ZoomHub.Storage.Internal.File        (toId)
import           ZoomHub.Types.Internal.Content       (Content, contentActive,
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
  , crCreatedAt      :: UTCTime
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
    field <*> field <*> field <*> field

instance ToRow ContentRow where
  toRow (ContentRow{..}) =
    [ toField crHashId
    , toField crUrl
    , toField crReady
    , toField crFailed
    , toField crProgress
    , toField crMime
    , toField crSize
    , toField crCreatedAt
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
    { crId = -1
    , crHashId = unId $ contentId c
    , crUrl = contentUrl c
    , crReady = contentReady c
    , crFailed = contentFailed c
    , crProgress = contentProgress c
    , crMime = contentMime c
    , crSize = contentSize c
    , crCreatedAt = contentCreatedAt c
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
    "url TEXT UNIQUE NOT NULL," <>
    "ready BOOLEAN DEFAULT 0 NOT NULL," <>
    "failed BOOLEAN DEFAULT 0 NOT NULL," <>
    "progress REAL DEFAULT 0.0 NOT NULL," <>
    "mime TEXT," <>
    "size INTEGER," <>
    "createdAt DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,"
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
    putStrLn "--- BEGIN ---"
    cd <- getCurrentDirectory
    let dataPath = cd </> "data"
    let idPath = dataPath </> "content-by-id"
    idFilenames <- filter (\f -> takeExtension f == ".json") <$>
      getDirectoryContents idPath
    let ids = map (fromString . toId . dropExtension) idFilenames
    conn <- open (dataPath </> "content.db")
    execute_ conn deleteContentTableQuery
    execute_ conn createContentTableQuery
    withTransaction conn (mapM_ (readAndInsert dataPath conn) ids)
    close conn
    putStrLn "--- DONE ---"
  where
    readAndInsert dataPath conn contentId = do
      let cId = unId contentId
      putStrLn $ "Processing " ++ cId
      maybeContent <- getById dataPath contentId
      case maybeContent of
        Nothing      -> putStrLn $ "Invalid ID: " ++ cId
        Just content -> insert conn content

    insert conn content = execute conn "INSERT INTO content (\
      \ hashId, url, ready, failed, progress, mime, size, createdAt, active, \
      \ activeAt, finishedAt, rawPath, dzi_width, dzi_height, dzi_tileSize, \
      \ dzi_tileOverlap, dzi_tileFormat) \
      \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)" (contentToRow content)
