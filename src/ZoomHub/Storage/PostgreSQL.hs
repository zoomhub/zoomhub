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
  -- , getExpiredActive
  -- ** Write operations
  -- , create
  -- , dequeueNextUnprocessed
  -- , markAsFailure
  -- , markAsSuccess
  -- , resetAsInitialized
  -- ** Misc
  -- , withConnection
  , PGS.ConnectInfo
  ) where

import           Data.Int                                (Int64)
import           Data.Profunctor.Product.Default         (Default)
import           Data.Profunctor.Product.TH              (makeAdaptorAndInstance)
import           Data.Text                               (Text)
import           Data.Time.Clock                         (UTCTime)
import qualified Database.PostgreSQL.Simple              as PGS
import           Opaleye                                 (Column, Nullable,
                                                          PGBool, PGFloat8,
                                                          PGInt4, PGInt8,
                                                          PGText, PGTimestamptz,
                                                          Query, QueryArr,
                                                          Table (Table),
                                                          Unpackspec, leftJoin,
                                                          limit, optional,
                                                          queryTable, required,
                                                          restrict, runQuery,
                                                          showSql, (.===))

-- import           ZoomHub.Log.Logger             (logWarning)
import           Control.Arrow                           (returnA)
import           ZoomHub.Types.Content                   (Content (Content),
                                                          contentActiveAt,
                                                          contentCompletedAt,
                                                          contentDZI,
                                                          contentError,
                                                          contentId,
                                                          contentInitializedAt,
                                                          contentMIME,
                                                          contentNumViews,
                                                          contentProgress,
                                                          contentSize,
                                                          contentState,
                                                          contentType,
                                                          contentURL, mkContent)
import           ZoomHub.Types.ContentId                 (ContentId, ContentId',
                                                          ContentIdColumn,
                                                          mkContentId,
                                                          pContentId)
import qualified ZoomHub.Types.ContentId                 as ContentId
import           ZoomHub.Types.ContentMIME               (ContentMIME, ContentMIME' (ContentMIME),
                                                          pContentMIME)
import           ZoomHub.Types.ContentState              (ContentState,
                                                          ContentStateColumn)
import           ZoomHub.Types.ContentType               (ContentType,
                                                          ContentTypeColumn)
import           ZoomHub.Types.ContentURI                (ContentURI, ContentURI' (ContentURI),
                                                          ContentURIColumn,
                                                          pContentURI)
import qualified ZoomHub.Types.ContentURI                as ContentURI
-- import           ZoomHub.Types.DatabasePath     (DatabasePath, unDatabasePath)
import           ZoomHub.Types.DeepZoomImage             (TileFormat,
                                                          TileOverlap, TileSize,
                                                          dziHeight,
                                                          dziTileFormat,
                                                          dziTileOverlap,
                                                          dziTileSize, dziWidth,
                                                          mkDeepZoomImage)
import qualified ZoomHub.Types.DeepZoomImage.TileFormat  as TileFormat
import qualified ZoomHub.Types.DeepZoomImage.TileOverlap as TileOverlap
import qualified ZoomHub.Types.DeepZoomImage.TileSize    as TileSize

-- Public API
create :: (Integer -> String) -> ContentURI -> PGS.Connection -> IO Content
create encodeId uri conn = undefined

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

-- get :: IO [ContentRow] -> IO (Maybe Content)
-- get queryAction = undefined

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql q = case showSql q of
  Just t  -> putStrLn t
  Nothing -> return ()

data ContentRow'
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
  = ContentRow
  { crId              :: tId
  , crHashId          :: tHashId
  , crTypeId          :: tTypeId
  , crURL             :: tURL
  , crState           :: tState
  , crInitializedAt   :: tInitializedAt
  , crActiveAt        :: tActiveAt
  , crCompletedAt     :: tCompletedAt
  , crTitle           :: tTitle
  , crAttributionText :: tAttributionText
  , crAttributionLink :: tAttributionLink
  , crMIME            :: tMIME
  , crSize            :: tSize
  , crError           :: tError
  , crProgress        :: tProgress
  , crAbuseLevelId    :: tAbuseLevelId
  , crNumAbuseReports :: tNumAbuseReports
  , crNumViews        :: tNumViews
  , crVersion         :: tVersion
  } deriving (Show)

type ContentRow = ContentRow'
  Int64               -- id
  ContentId           -- hashId
  ContentType         -- typeId
  ContentURI          -- url
  ContentState        -- state
  UTCTime             -- initializedAt
  (Maybe UTCTime)     -- activeAt
  (Maybe UTCTime)     -- completedAt
  (Maybe Text)        -- title
  (Maybe Text)        -- attributionText
  (Maybe Text)        -- attributionLink
  (Maybe ContentMIME) -- mime
  (Maybe Int64)       -- size
  (Maybe Text)        -- error
  Double              -- progress
  Int                 -- abuseLevelId
  Int64               -- numAbuseReports
  Int64               -- numViews
  Int                 -- version

type ContentRowWrite = ContentRow'
  (Maybe (Column PGInt8))           -- id
  ContentIdColumn                   -- hashId
  ContentTypeColumn                 -- typeId
  ContentURIColumn                  -- url
  ContentStateColumn                -- state
  (Column PGTimestamptz)            -- initializedAt
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

type ContentRowRead = ContentRow'
  (Column PGInt8)                   -- id
  ContentIdColumn                   -- hashId
  ContentTypeColumn                 -- typeId
  ContentURIColumn                  -- url
  ContentStateColumn                -- state
  (Column PGTimestamptz)            -- initializedAt
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

$(makeAdaptorAndInstance "pContent" ''ContentRow')

contentTable :: Table ContentRowWrite ContentRowRead
contentTable = Table "content"
  (pContent ContentRow
    { crId = optional "id"
    , crHashId = pContentId (mkContentId (required "hashid"))
    , crTypeId = required "typeid" -- TODO: Make type-safe using `newtype`
    , crURL = pContentURI (ContentURI (required "url"))
    , crState = required "state"   -- TODO: Make type-safe using `newtype`
    , crInitializedAt = required "initializedat"
    , crActiveAt = required "activeat"
    , crCompletedAt = required "completedat"
    , crTitle = required "title"
    , crAttributionText = required "attributiontext"
    , crAttributionLink = required "attributionlink"
    , crMIME = required "mime"
    , crSize = required "size"
    , crError = required "error"
    , crProgress = optional "progress"
    , crAbuseLevelId = optional "abuselevelid"
    , crNumAbuseReports = optional "numabusereports"
    , crNumViews = optional "numviews"
    , crVersion = optional "version"
    }
  )

-- Image
data ImageRow'
  tContentId
  tInitializedAt
  tWidth
  tHeight
  tTileSize
  tTileOverlap
  tTileFormat
  = ImageRow
  { imageContentId     :: tContentId
  , imageInitializedAt :: tInitializedAt
  , imageWidth         :: tWidth
  , imageHeight        :: tHeight
  , imageTileSize      :: tTileSize
  , imageTileOverlap   :: tTileOverlap
  , imageTileFormat    :: tTileFormat
  } deriving (Show)

type ImageRow = ImageRow'
  Int64   -- contentId
  UTCTime -- initializedAt
  Int64   -- width       -- TODO: Introduce type
  Int64   -- height      -- TODO: Introduce type
  Int64   -- tileSize    -- TODO: Introduce type
  Int64   -- tileOverlap -- TODO: Introduce type
  Text    -- tileFormat  -- TODO: Introduce type

type NullableImageRow = ImageRow'
  (Maybe Int64)   -- contentId
  (Maybe UTCTime) -- initializedAt
  (Maybe Int64)   -- width       -- TODO: Introduce type
  (Maybe Int64)   -- height      -- TODO: Introduce type
  (Maybe Int64)   -- tileSize    -- TODO: Introduce type
  (Maybe Int64)   -- tileOverlap -- TODO: Introduce type
  (Maybe Text)    -- tileFormat  -- TODO: Introduce type

type ImageRowReadWrite = ImageRow'
  (Column PGInt8)        -- contentId
  (Column PGTimestamptz) -- initializedAt
  (Column PGInt8)        -- width       -- TODO: Introduce type
  (Column PGInt8)        -- height      -- TODO: Introduce type
  (Column PGInt8)        -- tileSize    -- TODO: Introduce type
  (Column PGInt8)        -- tileOverlap -- TODO: Introduce type
  (Column PGText)        -- tileFormat  -- TODO: Introduce type

type NullableImageRowReadWrite = ImageRow'
  (Column (Nullable PGInt8))        -- contentId
  (Column (Nullable PGTimestamptz)) -- initializedAt
  (Column (Nullable PGInt8))        -- width       -- TODO: Introduce type
  (Column (Nullable PGInt8))        -- height      -- TODO: Introduce type
  (Column (Nullable PGInt8))        -- tileSize    -- TODO: Introduce type
  (Column (Nullable PGInt8))        -- tileOverlap -- TODO: Introduce type
  (Column (Nullable PGText))        -- tileFormat  -- TODO: Introduce type

$(makeAdaptorAndInstance "pImage" ''ImageRow')

imageTable :: Table ImageRowReadWrite ImageRowReadWrite
imageTable = Table "image"
  (pImage ImageRow
    { imageContentId = required "contentid"
    , imageInitializedAt = required "initializedat"
    , imageWidth = required "width"
    , imageHeight = required "height"
    , imageTileSize = required "tilesize"
    , imageTileOverlap = required "tileoverlap"
    , imageTileFormat = required "tileformat"
    }
  )

-- Query: Content
contentQuery :: Query ContentRowRead
contentQuery = queryTable contentTable

restrictContentId :: ContentId -> QueryArr ContentIdColumn ()
restrictContentId hashId = proc hashIdColumn -> do
    restrict -< hashIdColumn .=== ContentId.toColumn hashId

restrictContentURL :: ContentURI -> QueryArr ContentURIColumn ()
restrictContentURL url = proc uriColumn -> do
    restrict -< uriColumn .=== ContentURI.toColumn url

runContentQuery :: PGS.Connection -> Query ContentRowRead -> IO [ContentRow]
runContentQuery = runQuery

-- Query: Image
imageQuery :: Query ImageRowReadWrite
imageQuery = queryTable imageTable

runImageQuery :: PGS.Connection -> Query ImageRowReadWrite -> IO [ImageRow]
runImageQuery = runQuery

runContentImageQuery :: PGS.Connection ->
                        Query (ContentRowRead, NullableImageRowReadWrite) ->
                        IO [(ContentRow, NullableImageRow)]
runContentImageQuery = runQuery

-- Public API
getById :: ContentId -> PGS.Connection -> IO (Maybe Content)
getById hashId = getBy predicate
  where
    predicate = proc (cr, _) -> do
      restrictContentId hashId -< crHashId cr

getByURL :: ContentURI -> PGS.Connection -> IO (Maybe Content)
getByURL url = getBy predicate
  where
    predicate = proc (cr, _) -> do
      restrictContentURL url -< crURL cr

getBy :: (QueryArr (ContentRowRead, NullableImageRowReadWrite) ()) ->
         PGS.Connection ->
         IO (Maybe Content)
getBy predicate conn = do
    rs <- runContentImageQuery conn query
    case rs of
      [(cr, nir)] -> return . Just $ rowToContent cr nir
      _ -> return Nothing
  where
    query :: Query (ContentRowRead, NullableImageRowReadWrite)
    query = proc () -> do
      (cr, nir) <- leftJoin contentQuery imageQuery eqContentId -< ()
      predicate -< (cr, nir)
      returnA -< (cr, nir)

    eqContentId :: (ContentRowRead, ImageRowReadWrite) -> Column PGBool
    eqContentId (cr, ir) = crId cr .=== imageContentId ir

-- Helpers
rowToContent :: ContentRow -> NullableImageRow -> Content
rowToContent cr nir = Content
    { contentId = crHashId cr
    , contentType = crTypeId cr
    , contentURL = crURL cr
    , contentState = crState cr
    , contentInitializedAt = crInitializedAt cr
    , contentActiveAt = crActiveAt cr
    , contentCompletedAt = crCompletedAt cr
    , contentMIME = crMIME cr
    , contentSize = fromIntegral <$> (crSize cr)
    , contentProgress = crProgress cr
    , contentNumViews = fromIntegral (crNumViews cr)
    , contentError = crError cr
    , contentDZI = mDZI
    }
  where
    mDZIWidth = fromIntegral <$> imageWidth nir
    mDZIHeight = fromIntegral <$> imageHeight nir
    mDZITileSize = fromIntegral <$> imageTileSize nir >>= TileSize.fromInteger
    mDZITileOverlap = fromIntegral <$> imageTileOverlap nir >>=
      TileOverlap.fromInteger
    mDZITileFormat = imageTileFormat nir >>= TileFormat.fromText
    mDZI = mkDeepZoomImage <$> mDZIWidth <*> mDZIHeight <*> mDZITileSize <*>
      mDZITileOverlap <*> mDZITileFormat
