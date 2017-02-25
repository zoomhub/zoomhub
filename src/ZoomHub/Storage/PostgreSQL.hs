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
  , getByURL
  -- , getExpiredActive
  -- -- ** Write operations
  -- , create
  -- , dequeueNextUnprocessed
  -- , markAsFailure
  -- , markAsSuccess
  -- , resetAsInitialized
  -- ** Misc
  , createConnectionPool
  , PGS.ConnectInfo(..)
  , PGS.defaultConnectInfo
  -- ** Debug
  , printIncrNumViewsQuery
  ) where

import           Control.Arrow                           (returnA)
import           Data.Int                                (Int64)
import           Data.Pool                               (Pool, createPool)
import           Data.Profunctor.Product.TH              (makeAdaptorAndInstance)
import           Data.Text                               (Text)
import           Data.Time.Clock                         (NominalDiffTime,
                                                          UTCTime)
import           Data.Time.Units                         (Second, TimeUnit,
                                                          toMicroseconds)
import qualified Database.PostgreSQL.Simple              as PGS
import           Opaleye                                 (Column, Nullable,
                                                          PGBool, PGFloat8,
                                                          PGInt4, PGInt8,
                                                          PGText, PGTimestamptz,
                                                          Query, QueryArr,
                                                          Table (Table),
                                                          arrangeUpdateSql,
                                                          leftJoin, optional,
                                                          queryTable, required,
                                                          restrict, runQuery,
                                                          runUpdate, (.===))

-- import           ZoomHub.Log.Logger             (logWarning)
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
                                                          contentURL)
import           ZoomHub.Types.ContentId                 (ContentId,
                                                          ContentIdColumn,
                                                          mkContentId,
                                                          pContentId)
import qualified ZoomHub.Types.ContentId                 as ContentId
import           ZoomHub.Types.ContentMIME               (ContentMIME)
import           ZoomHub.Types.ContentState              (ContentState,
                                                          ContentStateColumn)
import           ZoomHub.Types.ContentType               (ContentType,
                                                          ContentTypeColumn)
import           ZoomHub.Types.ContentURI                (ContentURI, ContentURI' (ContentURI),
                                                          ContentURIColumn,
                                                          pContentURI)
import qualified ZoomHub.Types.ContentURI                as ContentURI
import           ZoomHub.Types.DeepZoomImage             (mkDeepZoomImage)
import qualified ZoomHub.Types.DeepZoomImage.TileFormat  as TileFormat
import qualified ZoomHub.Types.DeepZoomImage.TileOverlap as TileOverlap
import qualified ZoomHub.Types.DeepZoomImage.TileSize    as TileSize

-- -- Public API
-- create :: (Integer -> String) -> ContentURI -> PGS.Connection -> IO Content
-- create encodeId uri conn = undefined

-- getNextUnprocessed :: PGS.Connection -> IO (Maybe Content)
-- getNextUnprocessed conn = undefined

-- getExpiredActive :: PGS.Connection -> IO [Content]
-- getExpiredActive conn = undefined

-- -- Writes
-- dequeueNextUnprocessed :: PGS.Connection -> IO (Maybe Content)
-- dequeueNextUnprocessed conn = undefined

-- resetAsInitialized :: PGS.Connection -> [Content] -> IO ()
-- resetAsInitialized conn cs = undefined

-- markAsActive :: PGS.Connection -> Content -> IO Content
-- markAsActive conn content = undefined

-- markAsFailure :: PGS.Connection -> Content -> Maybe Text -> IO Content
-- markAsFailure conn content maybeError = undefined

-- markAsSuccess :: PGS.Connection -> Content -> IO Content
-- markAsSuccess conn content = undefined

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

-- Query: Image
imageQuery :: Query ImageRowReadWrite
imageQuery = queryTable imageTable

runContentImageQuery :: PGS.Connection ->
                        Query (ContentRowRead, NullableImageRowReadWrite) ->
                        IO [(ContentRow, NullableImageRow)]
runContentImageQuery = runQuery

-- Public API

-- Read
getById :: ContentId -> PGS.Connection -> IO (Maybe Content)
getById = getBy . hashIdRestriction

getById' :: ContentId -> PGS.Connection -> IO (Maybe Content)
getById' = getBy' . hashIdRestriction

getByURL :: ContentURI -> PGS.Connection -> IO (Maybe Content)
getByURL = getBy . urlRestriction

getByURL' :: ContentURI -> PGS.Connection -> IO (Maybe Content)
getByURL' = getBy' . urlRestriction

-- Helpers
createConnectionPool :: (TimeUnit a) =>
                        PGS.ConnectInfo ->
                        Integer ->
                        a ->
                        Integer ->
                        IO (Pool PGS.Connection)
createConnectionPool connInfo numStripes idleTime maxResourcesPerStripe =
  createPool (PGS.connect connInfo) PGS.close (fromIntegral numStripes)
    (toIdleTime idleTime) (fromIntegral maxResourcesPerStripe)

-- Helper
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

getBy' :: (QueryArr (ContentRowRead, NullableImageRowReadWrite) ()) ->
          PGS.Connection ->
          IO (Maybe Content)
getBy' predicate conn = do
    maybeContent <- getBy predicate conn
    case maybeContent of
      Just content -> do
        -- Sample how often we count views to reduce database load:
        -- http://stackoverflow.com/a/4762559/125305
        let numViews = contentNumViews content
        let numViewsSampleRate = sampleRate numViews
        numViewsSample <- randomRIO (1, numViewsSampleRate)
        when (numViewsSample == 1) $ do
          _ <- async $
            runIncrNumViewsQuery conn (contentId content) numViewsSample
          return ()
      Nothing -> return ()
    return maybeContent
  where
    sampleRate :: Integer -> Integer
    sampleRate numViews
      | numViews < 50   = 1
      | numViews < 500  = 10
      | numViews < 5000 = 20
      | otherwise       = 50

hashIdRestriction :: ContentId ->
                     (QueryArr (ContentRowRead, NullableImageRowReadWrite) ())
hashIdRestriction cHashId = proc (cr, _) -> do
  restrictContentId cHashId -< crHashId cr

urlRestriction :: ContentURI ->
                  (QueryArr (ContentRowRead, NullableImageRowReadWrite) ())
urlRestriction url = proc (cr, _) -> do
  restrictContentURL url -< crURL cr

type Op a columnsW columnsR =
  Table columnsW columnsR ->
  (columnsR -> columnsW) ->
  (columnsR -> Column PGBool) ->
  a

incrNumViewsQuery :: (Op a ContentRowWrite ContentRowRead) ->
                     ContentId ->
                     Integer ->
                     a
incrNumViewsQuery op cHashId numViewsSampleRate = op contentTable
  -- TODO: Why do we have to repeat optional fields?
  -- See: https://github.com/tomjaguarpaw/haskell-opaleye/issues/92
  (\cr -> cr
    { crId = Just $ crId cr
    , crProgress = Just $ crProgress cr
    , crAbuseLevelId = Just $ crAbuseLevelId cr
    , crNumAbuseReports = Just $ crNumAbuseReports cr
    , crNumViews = Just $ (crNumViews cr) + fromIntegral numViewsSampleRate
    , crVersion = Just $ crVersion cr
    }
  )
  -- TODO: Could we reuse `restrictByContentId` here?
  (\cr -> (crHashId cr) .=== ContentId.toColumn cHashId)

runIncrNumViewsQuery :: PGS.Connection -> ContentId -> Integer -> IO Int64
runIncrNumViewsQuery conn = incrNumViewsQuery (runUpdate conn)

printIncrNumViewsQuery :: ContentId -> Integer -> IO ()
printIncrNumViewsQuery cHashId numViewsSampleRate =
  putStrLn $ incrNumViewsQuery arrangeUpdateSql cHashId numViewsSampleRate

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

toIdleTime :: (TimeUnit a) => a -> NominalDiffTime
toIdleTime duration = fromIntegral $ toMicroseconds duration `div`
  toMicroseconds (1 :: Second)
