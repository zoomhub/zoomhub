{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Storage.SQLite
  (
  -- ** Read operations
    getById
  , getByURL
  , getNextUnprocessed
  -- ** Write operations
  , create
  , markAsActive
  , markAsFailure
  , markAsSuccess
  ) where

import           Control.Exception              (tryJust)
import           Control.Monad                  (guard)
import           Data.Aeson                     ((.=))
import           Data.Monoid                    ((<>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.String                    (IsString (fromString))
import           Data.Time.Clock                (UTCTime, getCurrentTime)
import           Database.SQLite.Simple         (Connection, NamedParam ((:=)),
                                                 Only (Only), Query, execute,
                                                 executeNamed, field, fromOnly,
                                                 query, query_, withTransaction)
import           Database.SQLite.Simple.FromRow (FromRow, fromRow)
import           Database.SQLite.Simple.ToField (toField)
import           Database.SQLite.Simple.ToRow   (ToRow, toRow)
import           Database.SQLite3               (Error (ErrorConstraint),
                                                 SQLError (..))

import           ZoomHub.Log.Logger             (logWarning)
import           ZoomHub.Types.Content          (Content (Content),
                                                 contentActiveAt,
                                                 contentCompletedAt, contentDZI,
                                                 contentId,
                                                 contentInitializedAt,
                                                 contentMIME, contentProgress,
                                                 contentSize, contentState,
                                                 contentURL, mkContent)
import           ZoomHub.Types.ContentId        (ContentId, unId)
import qualified ZoomHub.Types.ContentId        as ContentId
import           ZoomHub.Types.ContentMIME      (ContentMIME)
import           ZoomHub.Types.ContentState     (ContentState (Initialized, Active, CompletedSuccess, CompletedFailure))
import           ZoomHub.Types.ContentURI       (ContentURI)
import           ZoomHub.Types.DeepZoomImage    (DeepZoomImage (DeepZoomImage),
                                                 TileFormat, TileOverlap,
                                                 TileSize, dziHeight,
                                                 dziTileFormat, dziTileOverlap,
                                                 dziTileSize, dziWidth)
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
      let cId = ContentId.fromInteger encodeId newId
          content = mkContent cId uri
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
      logWarning "Failed to insert ID because it already exists" [
        "id" .= nId, "hashId" .= cId]

getById :: Connection -> ContentId -> IO (Maybe Content)
getById conn cId =
  getBy conn "hashId" (unId cId)

getByURL :: Connection -> ContentURI -> IO (Maybe Content)
getByURL conn uri =
  getBy conn "url" (show uri)

getNextUnprocessed :: Connection -> IO (Maybe Content)
getNextUnprocessed conn =
  get $ query conn "SELECT * FROM content WHERE state = ? AND\
    \ initializedAt IS NULL ORDER BY id ASC LIMIT 1"
    (Only Initialized)

markAsActive :: Connection -> Content -> IO Content
markAsActive conn content = do
  now <- getCurrentTime
  let content' = content
        { contentState = Active
        , contentActiveAt = Just now
        }
  withTransaction conn $
    executeNamed conn "UPDATE content \
      \ SET state = :state, activeAt = :activeAt WHERE hashId = :hashId"
      [ ":state" := contentState content'
      , ":activeAt" := contentActiveAt content'
      , ":hashId" := contentId content'
      ]
  return content'

markAsFailure :: Connection -> Content -> IO Content
markAsFailure conn content = do
  now <- getCurrentTime
  let content' = content
        { contentState = CompletedFailure
        , contentCompletedAt = Just now
        }
  withTransaction conn $
    executeNamed conn "UPDATE content \
      \ SET state = :state, completedAt = :completedAt WHERE hashId = :hashId"
      [ ":state" := contentState content'
      , ":completedAt" := contentCompletedAt content'
      , ":hashId" := contentId content'
      ]
  return content'

markAsSuccess :: Connection ->
                 Content ->
                 DeepZoomImage ->
                 Maybe ContentMIME ->
                 Integer ->
                 IO Content
markAsSuccess conn content dzi maybeMIME size = do
  now <- getCurrentTime
  let content' = content
        { contentState = CompletedSuccess
        , contentCompletedAt = Just now
        , contentMIME = maybeMIME
        , contentSize = Just size
        , contentProgress = 1.0
        , contentDZI = Just dzi
        }
  withTransaction conn $
    executeNamed conn "UPDATE content \
      \ SET state = :state\
      \   , completedAt = :completedAt\
      \   , mime = :mime\
      \   , size = :size\
      \   , progress = :progress\
      \   , dzi_width = :dzi_width\
      \   , dzi_height = :dzi_height\
      \   , dzi_tileSize = :dzi_tileSize\
      \   , dzi_tileOverlap = :dzi_tileOverlap\
      \   , dzi_tileFormat = :dzi_tileFormat\
      \ WHERE hashId = :hashId"
      [ ":hashId" := contentId content'
      , ":state" := contentState content'
      , ":completedAt" := contentCompletedAt content'
      , ":mime" := contentMIME content'
      , ":size" := contentSize content'
      , ":progress" := contentProgress content'
      , ":dzi_width" := Just (dziWidth dzi)
      , ":dzi_height" := Just (dziHeight dzi)
      , ":dzi_tileSize" := Just (dziTileSize dzi)
      , ":dzi_tileOverlap" := Just (dziTileOverlap dzi)
      , ":dzi_tileFormat" := Just (dziTileFormat dzi)
      ]
  return content'

-- Internal
getBy :: Connection -> String -> String -> IO (Maybe Content)
getBy conn fieldName param =
  get $ query conn (queryFor fieldName) (Only param)

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
  [ "id"
  , "hashId"
  , "url"
  , "state"
  , "initializedAt"
  , "activeAt"
  , "completedAt"
  , "mime"
  , "size"
  , "progress"
  , "dzi_width"
  , "dzi_height"
  , "dzi_tileSize"
  , "dzi_tileOverlap"
  , "dzi_tileFormat"
  ]

fieldNamesWithDefaults :: Set Query
fieldNamesWithDefaults = Set.fromList ["initializedAt"]

-- Filter out fields with default values
insertFieldNames :: [Query]
insertFieldNames =
  filter (`Set.notMember` fieldNamesWithDefaults) fieldNames

queryFor :: String -> Query
queryFor fieldName = "SELECT " <> columns <> " FROM content\
    \ WHERE " <> fromString fieldName <> " = ?"
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
  , crURL            :: ContentURI
  , crState          :: ContentState
  , crInitializedAt  :: Maybe UTCTime
  , crActiveAt       :: Maybe UTCTime
  , crCompletedAt    :: Maybe UTCTime
  , crMIME           :: Maybe ContentMIME
  , crSize           :: Maybe Integer
  , crProgress       :: Float
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
    field <*> -- url
    field <*> -- state
    field <*> -- initializedAt
    field <*> -- activeAt
    field <*> -- completedAt
    field <*> -- mime
    field <*> -- size
    field <*> -- progress
    field <*> -- dziWidth
    field <*> -- dziHeight
    field <*> -- dziTileSize
    field <*> -- dziTileOverlap
    field     -- dziTileFormat

instance ToRow ContentRow where
  toRow (ContentRow{..}) =
    [ toField crId
    , toField crHashId
    , toField crURL
    , toField crState
    -- , toField crInitializedAt -- Omitted due to SQLite default value
    , toField crActiveAt
    , toField crCompletedAt
    , toField crMIME
    , toField crSize
    , toField crProgress
    , toField crDZIWidth
    , toField crDZIHeight
    , toField crDZITileSize
    , toField crDZITileOverlap
    , toField crDZITileFormat
    ]

rowToContent :: ContentRow -> Content
rowToContent cr = Content
    { contentId = crHashId cr
    , contentURL = crURL cr
    , contentState = crState cr
    , contentInitializedAt = crInitializedAt cr
    , contentActiveAt = crActiveAt cr
    , contentCompletedAt = crCompletedAt cr
    , contentMIME = crMIME cr
    , contentSize = crSize cr
    , contentProgress = crProgress cr
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
        Just DeepZoomImage{..}
      _ -> Nothing

contentToRow :: Integer -> Content -> ContentRow
contentToRow id_ c = ContentRow
    { crId = Just id_
    , crHashId = contentId c
    , crURL = contentURL c
    , crState = contentState c
    , crInitializedAt = contentInitializedAt c
    , crActiveAt = contentActiveAt c
    , crCompletedAt = contentCompletedAt c
    , crMIME = contentMIME c
    , crSize = contentSize c
    , crProgress = contentProgress c
    , crDZIWidth = dziWidth <$> dzi
    , crDZIHeight = dziHeight <$> dzi
    , crDZITileSize = dziTileSize <$> dzi
    , crDZITileOverlap = dziTileOverlap  <$> dzi
    , crDZITileFormat = dziTileFormat  <$> dzi
    }
  where dzi = contentDZI c
