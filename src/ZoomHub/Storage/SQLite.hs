{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Storage.SQLite
  (
  -- ** Read operations
    getById
  , getById'
  , getByURL
  , getByURL'
  , getExpiredActive
  , getNextUnprocessed
  -- ** Write operations
  , create
  , markAsActive
  , markAsFailure
  , markAsSuccess
  , resetAsInitialized
  ) where

import           Control.Exception              (tryJust)
import           Control.Monad                  (forM_, guard)
import           Data.Aeson                     ((.=))
import           Data.Monoid                    ((<>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import           Data.Time.Clock                (UTCTime, getCurrentTime)
import           Database.SQLite.Simple         (Connection, NamedParam ((:=)),
                                                 Only (Only), Query, execute,
                                                 executeNamed, field, fromOnly,
                                                 query, queryNamed, query_,
                                                 withTransaction)
import           Database.SQLite.Simple.FromRow (FromRow, fromRow)
import           Database.SQLite.Simple.ToField (toField)
import           Database.SQLite.Simple.ToRow   (ToRow, toRow)
import           Database.SQLite3               (Error (ErrorConstraint),
                                                 SQLError (..))

import           ZoomHub.Log.Logger             (logWarning)
import           ZoomHub.Types.Content          (Content (Content),
                                                 contentActiveAt,
                                                 contentCompletedAt, contentDZI,
                                                 contentError, contentId,
                                                 contentInitializedAt,
                                                 contentMIME, contentProgress,
                                                 contentSize, contentState,
                                                 contentType, contentURL,
                                                 mkContent)
import           ZoomHub.Types.ContentId        (ContentId, unId)
import qualified ZoomHub.Types.ContentId        as ContentId
import           ZoomHub.Types.ContentMIME      (ContentMIME)
import           ZoomHub.Types.ContentState     (ContentState (Initialized, Active, CompletedSuccess, CompletedFailure))
import           ZoomHub.Types.ContentType      (ContentType (Unknown, Image))
import           ZoomHub.Types.ContentURI       (ContentURI)
import           ZoomHub.Types.DeepZoomImage    (DeepZoomImage, TileFormat,
                                                 TileOverlap, TileSize,
                                                 dziHeight, dziTileFormat,
                                                 dziTileOverlap, dziTileSize,
                                                 dziWidth, mkDeepZoomImage)
import           ZoomHub.Utils                  (intercalate)

-- Public API
create :: Connection -> (Integer -> String) -> ContentURI -> IO Content
create conn encodeId uri = withTransaction conn $ do
    (rowId:_) <- query_ conn lastContentRowInsertIdQuery :: IO [Only Integer]
    let newId = toInteger (fromOnly rowId) + 1
    insertWith newId
  where
    insertWith :: Integer -> IO Content
    insertWith newId = do
      initializedAt <- getCurrentTime
      let cId = ContentId.fromInteger encodeId newId
          -- TODO: Infer content type:
          content = mkContent Image cId uri initializedAt
      result <- tryJust (guard . isConstraintError) $
        execute conn insertQuery (contentToRow newId content)
      case result of
        Left _ -> do
          -- TODO: Implement proper logging:
          logWarnExistingId newId cId
          insertWith (newId + 1)
        Right _ -> return content

    isConstraintError :: SQLError -> Bool
    isConstraintError (SQLError ErrorConstraint _ _) = True
    isConstraintError _ = False

    logWarnExistingId :: Integer -> ContentId -> IO ()
    logWarnExistingId nId cId =
      logWarning "Failed to insert ID because it already exists"
        ["id" .= nId, "hashId" .= cId]

-- TODO: Generalize:
getById' :: Connection -> ContentId -> IO (Maybe Content)
getById' conn cId =
  getBy' conn "content.hashId" (unId cId)

getById :: Connection -> ContentId -> IO (Maybe Content)
getById conn cId =
  getBy conn "content.hashId" (unId cId)

-- TODO: Generalize:
getByURL :: Connection -> ContentURI -> IO (Maybe Content)
getByURL conn uri =
  getBy conn "content.url" (show uri)

getByURL' :: Connection -> ContentURI -> IO (Maybe Content)
getByURL' conn uri =
  getBy' conn "content.url" (show uri)

getNextUnprocessed :: Connection -> IO (Maybe Content)
getNextUnprocessed conn =
  get $ query conn (selectContent <> "WHERE state = ? \
    \ ORDER BY content.initializedAt ASC LIMIT 1")
    (Only Initialized)

getExpiredActive :: Connection -> IO [Content]
getExpiredActive conn = withTransaction conn $
  ((<$>) . (<$>)) rowToContent $ queryNamed conn
    (selectContent <> "WHERE content.state = :activeState AND\
     \ (julianday(datetime('now')) - julianday(datetime(content.activeAt))) \
     \ * 24 * 60 > :ttlMinutes")
      [ ":activeState" := Active
      , ":ttlMinutes" := (30 :: Integer)
      ]

resetAsInitialized :: Connection -> [Content] -> IO ()
resetAsInitialized conn cs =
  withTransaction conn $
    forM_ (map (unId . contentId) cs) $ \hashId ->
      executeNamed conn "UPDATE content \
      \ SET state = :initializedState, activeAt = NULL, error = NULL, \
      \ mime = NULL, size = NULL, progress = 0.0 WHERE hashId = :hashId"
      [ ":initializedState" := Initialized
      , ":hashId" := hashId
      ]

markAsActive :: Connection -> Content -> IO Content
markAsActive conn content = do
  activeAt <- getCurrentTime
  let content' = content
        { contentState = Active
        , contentActiveAt = Just activeAt
        }
  withTransaction conn $
    executeNamed conn "\
      \UPDATE content \
      \ SET state = :state\
      \   , activeAt = :activeAt\
      \ WHERE hashId = :hashId"
      [ ":state" := contentState content'
      , ":activeAt" := contentActiveAt content'
      , ":hashId" := contentId content'
      ]
  return content'

markAsFailure :: Connection ->
                 Content ->
                 Maybe Text ->
                 IO Content
markAsFailure conn content maybeError = do
  completedAt <- getCurrentTime
  let content' = content
        { contentState = CompletedFailure
        , contentType = Unknown
        , contentCompletedAt = Just completedAt
        , contentError = maybeError
        }
  withTransaction conn $
    executeNamed conn "\
      \UPDATE content\
      \  SET state = :state\
      \    , typeId = :typeId\
      \    , completedAt = :completedAt\
      \    , error = :error\
      \  WHERE hashId = :hashId"
      [ ":state" := contentState content'
      , ":typeId" := contentType content'
      , ":completedAt" := contentCompletedAt content'
      , ":hashId" := contentId content'
      , ":error" := contentError content'
      ]
  return content'

markAsSuccess :: Connection ->
                 Content ->
                 DeepZoomImage ->
                 Maybe ContentMIME ->
                 Integer ->
                 IO Content
markAsSuccess conn content dzi maybeMIME size = do
  completedAt <- getCurrentTime
  let content' = content
        { contentState = CompletedSuccess
        , contentType = Image -- TODO: Parametrize
        , contentCompletedAt = Just completedAt
        , contentMIME = maybeMIME
        , contentSize = Just size
        , contentProgress = 1.0
        , contentError = Nothing
        , contentDZI = Just dzi
        }
  withTransaction conn $ do
    executeNamed conn "\
      \ UPDATE content \
      \   SET state = :state\
      \     , typeId = :typeId\
      \     , completedAt = :completedAt\
      \     , mime = :mime\
      \     , size = :size\
      \     , progress = :progress\
      \     , error = :error\
      \   WHERE hashId = :hashId"
      [ ":hashId" := contentId content'
      , ":typeId" := contentType content'
      , ":state" := contentState content'
      , ":completedAt" := contentCompletedAt content'
      , ":mime" := contentMIME content'
      , ":size" := contentSize content'
      , ":progress" := contentProgress content'
      , ":error" := contentError content'
      ]
    executeNamed conn "\
      \ INSERT OR REPLACE INTO image\
      \    ( contentId\
      \    , initializedAt\
      \    , width\
      \    , height\
      \    , tileSize\
      \    , tileOverlap\
      \    , tileFormat\
      \    )\
      \ VALUES\
      \   ( (SELECT id FROM content WHERE hashId = :hashId)\
      \   , (SELECT initializedAt FROM image WHERE contentId =\
      \       (SELECT id FROM content WHERE hashId = :hashId))\
      \   , :image_width\
      \   , :image_height\
      \   , :image_tileSize\
      \   , :image_tileOverlap\
      \   , :image_tileFormat\
      \   )"
      [ ":hashId" := contentId content'
      , ":image_width" := Just (dziWidth dzi)
      , ":image_height" := Just (dziHeight dzi)
      , ":image_tileSize" := Just (dziTileSize dzi)
      , ":image_tileOverlap" := Just (dziTileOverlap dzi)
      , ":image_tileFormat" := Just (dziTileFormat dzi)
      ]
  return content'

-- Internal
getBy' :: Connection -> String -> String -> IO (Maybe Content)
getBy' conn fieldName param =
  withTransaction conn $ do
    execute conn (incrNumViewsQueryFor fieldName) (Only param)
    getBy conn fieldName param

getBy :: Connection -> String -> String -> IO (Maybe Content)
getBy conn fieldName param =
  get $ query conn (selectQueryFor fieldName) (Only param)

get :: IO [ContentRow] -> IO (Maybe Content)
get queryAction = do
  results <- queryAction
  case results of
    (r:_) -> return . Just . rowToContent $ r
    _     -> return Nothing

-- IMPORTANT:
-- The order of field names MUST match the definition of `ContentRow`:
fieldNames :: [Query]
fieldNames =
  [ "content.id"
  , "content.hashId"
  , "content.typeId"
  , "content.url"
  , "content.state"
  , "content.initializedAt"
  , "content.activeAt"
  , "content.completedAt"
  , "content.mime"
  , "content.size"
  , "content.progress"
  , "content.error"
  , "image.width AS image_width"
  , "image.height AS image_height"
  , "image.tileSize AS image_tileSize"
  , "image.tileOverlap AS image_tileOverlap"
  , "image.tileFormat AS image_tileFormat"
  ]

fieldNamesWithDefaults :: Set Query
fieldNamesWithDefaults = Set.fromList ["content.initializedAt"]

-- Filter out fields with default values
insertFieldNames :: [Query]
insertFieldNames =
  filter (`Set.notMember` fieldNamesWithDefaults) fieldNames

selectQueryFor :: String -> Query
selectQueryFor fieldName =
  selectContent <> "WHERE " <> fromString fieldName <> " = ?"

incrNumViewsQueryFor :: String -> Query
incrNumViewsQueryFor fieldName =
  "UPDATE content SET numViews = numViews + 1 WHERE " <>
    fromString fieldName <> " = ?"

selectContent :: Query
selectContent =
    "SELECT " <> columns <> " FROM content\
    \ LEFT JOIN image ON content.id = image.contentId "
  where
    columns = intercalate ", " fieldNames

insertQuery :: Query
insertQuery = "INSERT INTO content (" <> columns <> ")\
    \ VALUES (" <> placeholders <> ")"
  where
    names = insertFieldNames
    columns = intercalate ", " names
    placeholders = intercalate ", " (map (const "?") names)

lastContentRowInsertIdQuery :: Query
lastContentRowInsertIdQuery =
  "SELECT seq FROM sqlite_sequence WHERE name=\"content\""

data ContentRow = ContentRow
  { crId             :: Maybe Integer
  , crHashId         :: ContentId
  , crType           :: ContentType
  , crURL            :: ContentURI
  , crState          :: ContentState
  , crInitializedAt  :: UTCTime
  , crActiveAt       :: Maybe UTCTime
  , crCompletedAt    :: Maybe UTCTime
  , crMIME           :: Maybe ContentMIME
  , crSize           :: Maybe Integer
  , crProgress       :: Float
  , crError          :: Maybe Text
  , crDZIWidth       :: Maybe Integer
  , crDZIHeight      :: Maybe Integer
  , crDZITileSize    :: Maybe TileSize
  , crDZITileOverlap :: Maybe TileOverlap
  , crDZITileFormat  :: Maybe TileFormat
  } deriving (Show)

instance FromRow ContentRow where
  fromRow = ContentRow <$>
    field <*> -- id
    field <*> -- hashId
    field <*> -- type
    field <*> -- url
    field <*> -- state
    field <*> -- initializedAt
    field <*> -- activeAt
    field <*> -- completedAt
    field <*> -- mime
    field <*> -- size
    field <*> -- progress
    field <*> -- error
    field <*> -- dziWidth
    field <*> -- dziHeight
    field <*> -- dziTileSize
    field <*> -- dziTileOverlap
    field     -- dziTileFormat

instance ToRow ContentRow where
  toRow (ContentRow{..}) =
    [ toField crId
    , toField crHashId
    , toField crType
    , toField crURL
    , toField crState
    -- , toField crInitializedAt -- Omitted due to SQLite default value
    , toField crActiveAt
    , toField crCompletedAt
    , toField crMIME
    , toField crSize
    , toField crProgress
    , toField crError
    , toField crDZIWidth
    , toField crDZIHeight
    , toField crDZITileSize
    , toField crDZITileOverlap
    , toField crDZITileFormat
    ]

rowToContent :: ContentRow -> Content
rowToContent cr = Content
    { contentId = crHashId cr
    , contentType = crType cr
    , contentURL = crURL cr
    , contentState = crState cr
    , contentInitializedAt = crInitializedAt cr
    , contentActiveAt = crActiveAt cr
    , contentCompletedAt = crCompletedAt cr
    , contentMIME = crMIME cr
    , contentSize = crSize cr
    , contentProgress = crProgress cr
    , contentError = crError cr
    , contentDZI = maybeDZI
    }
  where
    maybeDZIWidth = crDZIWidth cr
    maybeDZIHeight = crDZIHeight cr
    maybeDZITileSize = crDZITileSize cr
    maybeDZITileOverlap = crDZITileOverlap cr
    maybeDZITileFormat = crDZITileFormat cr
    maybeDZI =
      case (maybeDZIWidth, maybeDZIHeight, maybeDZITileSize,
            maybeDZITileOverlap, maybeDZITileFormat) of
      (Just dziWidth, Just dziHeight,
       Just dziTileSize, Just dziTileOverlap, Just dziTileFormat) ->
        Just $ mkDeepZoomImage dziWidth dziHeight dziTileSize
          dziTileOverlap dziTileFormat
      _ -> Nothing

contentToRow :: Integer -> Content -> ContentRow
contentToRow id_ c = ContentRow
    { crId = Just id_
    , crHashId = contentId c
    , crType = contentType c
    , crURL = contentURL c
    , crState = contentState c
    , crInitializedAt = contentInitializedAt c
    , crActiveAt = contentActiveAt c
    , crCompletedAt = contentCompletedAt c
    , crMIME = contentMIME c
    , crSize = contentSize c
    , crProgress = contentProgress c
    , crError = contentError c
    , crDZIWidth = dziWidth <$> dzi
    , crDZIHeight = dziHeight <$> dzi
    , crDZITileSize = dziTileSize <$> dzi
    , crDZITileOverlap = dziTileOverlap  <$> dzi
    , crDZITileFormat = dziTileFormat  <$> dzi
    }
  where dzi = contentDZI c
