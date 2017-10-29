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
  , getById'
  , getByURL
  , getByURL'
  , getExpiredActive
  -- ** Write operations
  , create
  -- , dequeueNextUnprocessed
  -- , markAsFailure
  -- , markAsSuccess
  -- , resetAsInitialized
  -- ** Misc
  , createConnectionPool
  , PGS.ConnectInfo(..)
  , PGS.defaultConnectInfo
  -- ** Debug (TODO: Move to `Internal`)
  , expiredActiveQuery
  , getNextUnprocessed
  , lastContentRowInsertIdQuery
  , printIncrNumViewsQuery
  , runInsertContent
  ) where

import           Control.Arrow                           (returnA)
import           Control.Concurrent.Async                (async)
import           Control.Exception                       (tryJust)
import           Control.Monad                           (guard, when)
import           Data.Aeson                              ((.=))
import           Data.Int                                (Int64)
import           Data.Monoid                             ((<>))
import           Data.Profunctor.Product.TH              (makeAdaptorAndInstance)
import           Data.Text                               (Text)
import           Data.Time.Clock                         (UTCTime,
                                                          getCurrentTime)
import           Data.Time.Units                         (Minute)
import qualified Database.PostgreSQL.Simple              as PGS
import qualified Database.PostgreSQL.Simple.Errors       as PGS
import           Opaleye                                 (Column, Nullable,
                                                          PGBool, PGFloat8,
                                                          PGInt4, PGInt8,
                                                          PGText, PGTimestamptz,
                                                          Query, QueryArr,
                                                          Table (Table),
                                                          arrangeUpdateSql, asc,
                                                          constant, desc,
                                                          leftJoin, limit,
                                                          matchNullable,
                                                          optional, orderBy,
                                                          pgBool, pgUTCTime,
                                                          queryTable, required,
                                                          restrict, runInsert,
                                                          runQuery, runUpdate,
                                                          (.<=), (.===))
import           System.Random                           (randomRIO)

import           ZoomHub.Log.Logger                      (logWarning)
import           ZoomHub.Storage.PostgreSQL.Internal     (createConnectionPool,
                                                          subtractUTCTime,
                                                          toNominalDiffTime)
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
import           ZoomHub.Types.ContentId                 (ContentId,
                                                          ContentIdColumn,
                                                          mkContentId,
                                                          pContentId)
import qualified ZoomHub.Types.ContentId                 as ContentId
import           ZoomHub.Types.ContentMIME               (ContentMIME)
import           ZoomHub.Types.ContentState              as ContentState
import           ZoomHub.Types.ContentType               as ContentType
import           ZoomHub.Types.ContentURI                (ContentURI, ContentURI' (ContentURI),
                                                          ContentURIColumn,
                                                          pContentURI)
import qualified ZoomHub.Types.ContentURI                as ContentURI
import           ZoomHub.Types.DeepZoomImage             (mkDeepZoomImage)
import qualified ZoomHub.Types.DeepZoomImage.TileFormat  as TileFormat
import qualified ZoomHub.Types.DeepZoomImage.TileOverlap as TileOverlap
import qualified ZoomHub.Types.DeepZoomImage.TileSize    as TileSize

-- Public API

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
  (Maybe (Column PGTimestamptz) )           -- initializedAt
  (Maybe (Column (Nullable PGTimestamptz))) -- activeAt
  (Maybe (Column (Nullable PGTimestamptz))) -- completedAt
  (Maybe (Column (Nullable PGText)))        -- title
  (Maybe (Column (Nullable PGText)))        -- attributionText
  (Maybe (Column (Nullable PGText)))        -- attributionLink
  (Maybe (Column (Nullable PGText)))        -- mime
  (Maybe (Column (Nullable PGInt8)))        -- size
  (Maybe (Column (Nullable PGText)))        -- error
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
    , crHashId = pContentId (mkContentId (required "hash_id"))
    , crTypeId = required "type_id" -- TODO: Make type-safe using `newtype`
    , crURL = pContentURI (ContentURI (required "url"))
    , crState = required "state"   -- TODO: Make type-safe using `newtype`
    , crInitializedAt = optional "initialized_at"
    , crActiveAt = optional "active_at"
    , crCompletedAt = optional "completed_at"
    , crTitle = optional "title"
    , crAttributionText = optional "attribution_text"
    , crAttributionLink = optional "attribution_link"
    , crMIME = optional "mime"
    , crSize = optional "size"
    , crError = optional "error"
    , crProgress = optional "progress"
    , crAbuseLevelId = optional "abuse_level_id"
    , crNumAbuseReports = optional "num_abuse_reports"
    , crNumViews = optional "num_views"
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
    { imageContentId = required "content_id"
    , imageInitializedAt = required "initialized_at"
    , imageWidth = required "width"
    , imageHeight = required "height"
    , imageTileSize = required "tile_size"
    , imageTileOverlap = required "tile_overlap"
    , imageTileFormat = required "tile_format"
    }
  )

-- Query: Content
contentQuery :: Query ContentRowRead
contentQuery = queryTable contentTable

restrictContentId :: ContentId -> QueryArr ContentIdColumn ()
restrictContentId hashId = proc hashIdColumn ->
    restrict -< hashIdColumn .=== ContentId.toColumn hashId

restrictContentURL :: ContentURI -> QueryArr ContentURIColumn ()
restrictContentURL url = proc uriColumn ->
    restrict -< uriColumn .=== ContentURI.toColumn url

restrictContentState :: ContentState -> QueryArr ContentStateColumn ()
restrictContentState state = proc stateColumn ->
    restrict -< stateColumn .=== ContentState.toColumn state

-- Manipulation: Content
runInsertContent :: PGS.Connection -> Content -> IO Int64
runInsertContent conn content =  runInsert conn contentTable (contentToRow content)

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

-- Worker
getNextUnprocessed :: PGS.Connection -> IO (Maybe Content)
getNextUnprocessed conn = do
    rs <- runQuery conn nextUnprocessedQuery
    case rs of
      [r] -> return . Just $ rowToContent r
      _   -> return Nothing

nextUnprocessedQuery :: Query ContentRowRead
nextUnprocessedQuery = first $ proc () -> do
    cs <- query -< ()
    stateRestriction ContentState.Initialized -< cs
    returnA -< cs
  where
    query :: Query ContentRowRead
    query = orderBy (mostPopularFirst <> oldestFirst) $ contentQuery

    first = limit 1
    mostPopularFirst = desc crNumViews
    oldestFirst = asc crInitializedAt

getExpiredActive :: PGS.Connection -> IO [Content]
getExpiredActive conn = do
    currentTime <- getCurrentTime
    let ttl = toNominalDiffTime (30 :: Minute)
        lastActiveTime = subtractUTCTime currentTime ttl
    rs <- runQuery conn (expiredActiveQuery lastActiveTime)
    return $ map rowToContent rs

expiredActiveQuery :: UTCTime -> Query ContentRowRead
expiredActiveQuery lastActiveTime = proc () -> do
    cs <- contentQuery -< ()
    stateRestriction ContentState.Active -< cs
    restrict -< isExpired cs lastActiveTime
    returnA -< cs
  where
    isExpired :: ContentRowRead -> UTCTime -> Column PGBool
    isExpired cr cutOffTime =
      let activeAt = crActiveAt cr
          defaultValue = pgBool False
      in
      matchNullable defaultValue (.<= pgUTCTime cutOffTime) activeAt

-- Helper
contentToRow :: Content -> ContentRowWrite
contentToRow c = ContentRow
  { crId = Nothing
  , crHashId = constant $ contentId c
  , crTypeId = constant $ contentType c
  , crURL = constant $ contentURL c
  , crState = constant $ contentState c
  , crInitializedAt = Just . constant $ contentInitializedAt c
  , crActiveAt = Just . constant $ contentActiveAt c
  , crCompletedAt = Just . constant $ contentCompletedAt c
  , crTitle = Nothing
  , crAttributionText = Nothing
  , crAttributionLink = Nothing
  , crMIME = Just . constant $ contentMIME c
  , crSize = Just . constant $ (fromInteger <$> contentSize c :: Maybe Int64)
  , crError = Just . constant $ contentError c
  , crProgress = Just . constant $ contentProgress c
  , crAbuseLevelId = Nothing
  , crNumAbuseReports = Nothing
  , crNumViews = Just . constant $ (fromInteger . contentNumViews $ c :: Int64)
  , crVersion = Nothing
  }

rowToContent :: ContentRow -> Content
rowToContent cr = Content
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
    , contentDZI = Nothing
    }

rowWithImageToContent :: ContentRow -> NullableImageRow -> Content
rowWithImageToContent cr nir =
    let content = (rowToContent cr) in
    content { contentDZI = mDZI }
  where
    mDZIWidth = fromIntegral <$> imageWidth nir
    mDZIHeight = fromIntegral <$> imageHeight nir
    mDZITileSize = fromIntegral <$> imageTileSize nir >>= TileSize.fromInteger
    mDZITileOverlap = fromIntegral <$> imageTileOverlap nir >>=
      TileOverlap.fromInteger
    mDZITileFormat = imageTileFormat nir >>= TileFormat.fromText
    mDZI = mkDeepZoomImage <$> mDZIWidth <*> mDZIHeight <*> mDZITileSize <*>
      mDZITileOverlap <*> mDZITileFormat

getBy :: (QueryArr (ContentRowRead, NullableImageRowReadWrite) ()) ->
         PGS.Connection ->
         IO (Maybe Content)
getBy predicate conn = do
    rs <- runContentImageQuery conn query
    case rs of
      [(cr, nir)] -> return . Just $ rowWithImageToContent cr nir
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
hashIdRestriction cHashId = proc (cr, _) ->
  restrictContentId cHashId -< crHashId cr

urlRestriction :: ContentURI ->
                  (QueryArr (ContentRowRead, NullableImageRowReadWrite) ())
urlRestriction url = proc (cr, _) ->
  restrictContentURL url -< crURL cr

stateRestriction :: ContentState -> QueryArr ContentRowRead ()
stateRestriction state = proc cr ->
  restrictContentState state -< crState cr

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
    , crInitializedAt = Just $ crInitializedAt cr
    , crActiveAt = Just $ crActiveAt cr
    , crCompletedAt = Just $ crCompletedAt cr
    , crTitle = Just $ crTitle cr
    , crAttributionText = Just $ crAttributionText cr
    , crAttributionLink = Just $ crAttributionLink cr
    , crMIME = Just $ crMIME cr
    , crSize = Just $ crSize cr
    , crError = Just $ crError cr
    , crProgress = Just $ crProgress cr
    , crAbuseLevelId = Just $ crAbuseLevelId cr
    , crNumAbuseReports = Just $ crNumAbuseReports cr
    , crVersion = Just $ crVersion cr

    -- Actual update:
    , crNumViews = Just $ (crNumViews cr) + fromIntegral numViewsSampleRate
    }
  )
  -- TODO: Could we reuse `restrictContentId` here?
  (\cr -> (crHashId cr) .=== ContentId.toColumn cHashId)

runIncrNumViewsQuery :: PGS.Connection -> ContentId -> Integer -> IO Int64
runIncrNumViewsQuery conn = incrNumViewsQuery (runUpdate conn)

printIncrNumViewsQuery :: ContentId -> Integer -> IO ()
printIncrNumViewsQuery cHashId numViewsSampleRate =
  putStrLn $ incrNumViewsQuery arrangeUpdateSql cHashId numViewsSampleRate

-- `content_id_seq`
data ContentSeqRow' tLastValue = ContentSeqRow
  { csLastValue :: tLastValue
  } deriving Show

$(makeAdaptorAndInstance "pContentSeq" ''ContentSeqRow')

type ContentSeqRow = ContentSeqRow'
  Int64           -- last_value

type ContentSeqRowReadWrite = ContentSeqRow'
  (Column PGInt8) -- last_value

contentSeqTable :: Table ContentSeqRowReadWrite ContentSeqRowReadWrite
contentSeqTable = Table "content_id_seq"
  (pContentSeq ContentSeqRow
    { csLastValue = required "last_value"
    }
  )

contentSeqQuery :: Query ContentSeqRowReadWrite
contentSeqQuery = queryTable contentSeqTable

runContentSeqQuery :: PGS.Connection ->
                      Query ContentSeqRowReadWrite ->
                      IO [ContentSeqRow]
runContentSeqQuery = runQuery

lastContentRowInsertIdQuery :: Query ContentSeqRowReadWrite
lastContentRowInsertIdQuery = proc () -> do
  cs <- contentSeqQuery -< ()
  returnA -< cs

-- Write
create :: (Integer -> String) -> ContentURI -> PGS.Connection -> IO Content
create encodeId uri conn = PGS.withTransaction conn $ do
    (r:_) <- runContentSeqQuery conn lastContentRowInsertIdQuery
    let newId = toInteger $ csLastValue r + 1
    insertWith newId
  where
    insertWith :: Integer -> IO Content
    insertWith newId = do
      initializedAt <- getCurrentTime
      let cId = ContentId.fromInteger encodeId newId
          -- TODO: Infer content type:
          content = mkContent ContentType.Image cId uri initializedAt
      result <- tryJust (guard . isConstraintError) $
        runInsertContent conn content
      case result of
        Left _ -> do
          -- TODO: Implement proper logging:
          logWarnExistingId newId cId
          -- TODO: Prevent potentially infinite recursion:
          insertWith (newId + 1)
        Right _ -> return content

    isConstraintError :: PGS.SqlError -> Bool
    isConstraintError e = case PGS.constraintViolation e of
      Just (PGS.CheckViolation _ _) -> True
      _ -> False

    logWarnExistingId :: Integer -> ContentId -> IO ()
    logWarnExistingId dbId cId =
      logWarning "Failed to insert ID because it already exists"
        [ "dbId" .= dbId
        , "id" .= cId
        ]
