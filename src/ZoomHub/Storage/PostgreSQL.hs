{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZoomHub.Storage.PostgreSQL
  (
  -- ** Read operations
    getById
  -- , getById'
  , getByURL
  -- , getByURL'
  , getExpiredActive
  -- ** Write operations
  , create
  , dequeueNextUnprocessed
  , markAsFailure
  , markAsSuccess
  , resetAsInitialized
  -- ** Misc
  -- , withConnection
  , main
  ) where

import           Data.Int                        (Int64)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Text                       (Text)
import           Data.Text                       (Text)
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Clock                 (UTCTime)
import qualified Database.PostgreSQL.Simple      as PGS
import           Opaleye                         (Column, Nullable, PGFloat8,
                                                  PGInt4, PGInt8, PGText,
                                                  PGTimestamptz, Query,
                                                  Table (Table), Unpackspec,
                                                  limit, optional, queryTable,
                                                  required, runQuery,
                                                  showSql)

-- import           ZoomHub.Log.Logger             (logWarning)
-- import           ZoomHub.Types.Content          (Content (Content),
--                                                  contentActiveAt,
--                                                  contentCompletedAt, contentDZI,
--                                                  contentError, contentId,
--                                                  contentInitializedAt,
--                                                  contentMIME, contentNumViews,
--                                                  contentProgress, contentSize,
--                                                  contentState, contentType,
--                                                  contentURL, mkContent)
import           ZoomHub.Types.ContentId         (ContentId,
                                                  ContentId' (ContentId),
                                                  ContentIdColumn, pContentId,
                                                  unId)
-- import qualified ZoomHub.Types.ContentId        as ContentId
-- import           ZoomHub.Types.ContentMIME      (ContentMIME)
-- import           ZoomHub.Types.ContentState     (ContentState (Initialized, Active, CompletedSuccess, CompletedFailure))
import           ZoomHub.Types.ContentType       (ContentType,
                                                  ContentTypeColumn)
import           ZoomHub.Types.ContentURI        (ContentURI)
-- import           ZoomHub.Types.DatabasePath     (DatabasePath, unDatabasePath)
-- import           ZoomHub.Types.DeepZoomImage    (TileFormat, TileOverlap,
--                                                  TileSize, dziHeight,
--                                                  dziTileFormat, dziTileOverlap,
--                                                  dziTileSize, dziWidth,
--                                                  mkDeepZoomImage)

-- Public API
create :: (Integer -> String) -> ContentURI -> PGS.Connection -> IO Content
create encodeId uri conn = undefined

getById :: ContentId -> PGS.Connection -> IO (Maybe Content)
getById cId = undefined

getByURL :: ContentURI -> PGS.Connection -> IO (Maybe Content)
getByURL uri = undefined

getNextUnprocessed :: PGS.Connection -> IO (Maybe Content)
getNextUnprocessed conn = undefined

getExpiredActive :: PGS.Connection -> IO [Content]
getExpiredActive conn = undefined

-- Writes
dequeueNextUnprocessed :: PGS.Connection -> IO (Maybe Content)
dequeueNextUnprocessed conn = undefined

resetAsInitialized :: PGS.Connection -> [Content] -> IO ()
resetAsInitialized conn cs = undefined

markAsActive :: PGS.Connection -> Content -> IO Content
markAsActive conn content = undefined

markAsFailure :: PGS.Connection -> Content -> Maybe Text -> IO Content
markAsFailure conn content maybeError = undefined

markAsSuccess :: PGS.Connection -> Content -> IO Content
markAsSuccess conn content = undefined

getBy :: String -> String -> PGS.Connection -> IO (Maybe Content)
getBy fieldName param conn = undefined

-- get :: IO [ContentRow] -> IO (Maybe Content)
-- get queryAction = undefined

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql q = case showSql q of
  Just t  -> putStrLn t
  Nothing -> return ()

data Content'
  tId
  tHashId
  tTypeId
  tURL
  tState
  tInitializedAt
  tActiveAt
  tCompletedAt
  tTitle
  tAttributionText
  tAttributionLink
  tMIME
  tSize
  tError
  tProgress
  tAbuseLevelId
  tNumAbuseReports
  tNumViews
  tVersion
  = Content
  { contentId              :: tId
  , contentHashId          :: tHashId
  , contentTypeId          :: tTypeId
  , contentURL             :: tURL
  , contentState           :: tState
  , contentInitializedAt   :: tInitializedAt
  , contentActiveAt        :: tActiveAt
  , contentCompletedAt     :: tCompletedAt
  , contentTitle           :: tTitle
  , contentAttributionText :: tAttributionText
  , contentAttributionLink :: tAttributionLink
  , contentMIME            :: tMIME
  , contentSize            :: tSize
  , contentError           :: tError
  , contentProgress        :: tProgress
  , contentAbuseLevelId    :: tAbuseLevelId
  , contentNumAbuseReports :: tNumAbuseReports
  , contentNumViews        :: tNumViews
  , contentVersion         :: tVersion
  } deriving (Show)

type Content = Content'
  Int64           -- id
  ContentId       -- hashId
  ContentType     -- typeId
  Text            -- url
  Text            -- state
  (Maybe UTCTime) -- initializedAt
  (Maybe UTCTime) -- activeAt
  (Maybe UTCTime) -- completedAt
  (Maybe Text)    -- title
  (Maybe Text)    -- attributionText
  (Maybe Text)    -- attributionLink
  (Maybe Text)    -- mime
  (Maybe Int64)   -- size
  (Maybe Text)    -- error
  Double          -- progress
  Int             -- abuseLevelId
  Int64           -- numAbuseReports
  Int64           -- numViews
  Int             -- version

type ContentColumnWrite = Content'
  (Maybe (Column PGInt8))           -- id
  ContentIdColumn                   -- hashId
  ContentTypeColumn                 -- typeId
  (Column PGText)                   -- url
  (Column PGText)                   -- state
  (Column (Nullable PGTimestamptz)) -- initializedAt
  (Column (Nullable PGTimestamptz)) -- activeAt
  (Column (Nullable PGTimestamptz)) -- completedAt
  (Column (Nullable PGText))        -- title
  (Column (Nullable PGText))        -- attributionText
  (Column (Nullable PGText))        -- attributionLink
  (Column (Nullable PGText))        -- mime
  (Column (Nullable PGInt8))        -- size
  (Column (Nullable PGText))        -- error
  (Maybe (Column PGFloat8))         -- progress
  (Maybe (Column PGInt4))           -- abuseLevelId
  (Maybe (Column PGInt8))           -- numAbuseReports
  (Maybe (Column PGInt8))           -- numViews
  (Maybe (Column PGInt4))           -- version

type ContentColumnRead = Content'
  (Column PGInt8)                   -- id
  ContentIdColumn                   -- hashId
  ContentTypeColumn                 -- typeId
  (Column PGText)                   -- url
  (Column PGText)                   -- state
  (Column (Nullable PGTimestamptz)) -- initializedAt
  (Column (Nullable PGTimestamptz)) -- activeAt
  (Column (Nullable PGTimestamptz)) -- completedAt
  (Column (Nullable PGText))        -- title
  (Column (Nullable PGText))        -- attributionText
  (Column (Nullable PGText))        -- attributionLink
  (Column (Nullable PGText))        -- mime
  (Column (Nullable PGInt8))        -- size
  (Column (Nullable PGText))        -- error
  (Column PGFloat8)                 -- progress
  (Column PGInt4)                   -- abuseLevelId
  (Column PGInt8)                   -- numAbuseReports
  (Column PGInt8)                   -- numViews
  (Column PGInt4)                   -- version

$(makeAdaptorAndInstance "pContent" ''Content')

contentTable :: Table ContentColumnWrite ContentColumnRead
contentTable = Table "content"
                   (pContent Content
                      { contentId = optional "id"
                      , contentHashId = pContentId (ContentId (required "hashid"))
                      , contentTypeId = required "typeid"
                      , contentURL = required "url"
                      , contentState = required "state"
                      , contentInitializedAt = required "initializedat"
                      , contentActiveAt = required "activeat"
                      , contentCompletedAt = required "completedat"
                      , contentTitle = required "title"
                      , contentAttributionText = required "attributiontext"
                      , contentAttributionLink = required "attributionlink"
                      , contentMIME = required "mime"
                      , contentSize = required "size"
                      , contentError = required "error"
                      , contentProgress = optional "progress"
                      , contentAbuseLevelId = optional "abuselevelid"
                      , contentNumAbuseReports = optional "numabusereports"
                      , contentNumViews = optional "numviews"
                      , contentVersion = optional "version"
                      }
                   )

contentQuery :: Query ContentColumnRead
contentQuery = queryTable contentTable

runContentQuery :: PGS.Connection -> Query ContentColumnRead -> IO [Content]
runContentQuery = runQuery

dbConnectInfo :: PGS.ConnectInfo
dbConnectInfo = PGS.defaultConnectInfo { PGS.connectDatabase = "zoomhub-production" }

main :: IO ()
main = do
  -- printSql contentQuery
  conn <- PGS.connect dbConnectInfo
  res <- runContentQuery conn (limit 1 contentQuery)
  print $ res
