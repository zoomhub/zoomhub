{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Storage.SQLite
  ( create
  , getById
  , getByURL
  ) where

import           Control.Exception              (tryJust)
import           Control.Monad                  (guard)
import           Data.Aeson                     ((.=))
import           Data.Monoid                    ((<>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.String                    (IsString (fromString))
import           Data.Time.Clock                (UTCTime)
import           Database.SQLite.Simple         (Connection, Only (Only), Query,
                                                 execute, field, fromOnly,
                                                 query, query_, withTransaction)
import           Database.SQLite.Simple.FromRow (FromRow, fromRow)
import           Database.SQLite.Simple.ToField (toField)
import           Database.SQLite.Simple.ToRow   (ToRow, toRow)
import           Database.SQLite3               (Error (ErrorConstraint),
                                                 SQLError (..))

import           ZoomHub.Log.Logger             (logWarning)
import           ZoomHub.Types.Content          (Content (Content),
                                                 contentActiveAt,
                                                 contentCompletedAt, contentDzi,
                                                 contentId,
                                                 contentInitializedAt,
                                                 contentMime, contentProgress,
                                                 contentSize, contentState,
                                                 contentUrl, mkContent)
import           ZoomHub.Types.ContentId        (ContentId, unId)
import qualified ZoomHub.Types.ContentId        as ContentId
import           ZoomHub.Types.ContentState     (ContentState)
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

tableName :: Query
tableName = "content"

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
queryFor fieldName = "SELECT " <> columns <> " FROM " <> tableName <> "\
    \ WHERE " <> fromString fieldName <> " = ?"
  where
    columns = intercalate ", " fieldNames

insertQuery :: Query
insertQuery = "INSERT INTO " <> tableName <> " (" <> columns <> ")\
    \ VALUES (" <> placeholders <> ")"
  where
    names = insertFieldNames
    columns = intercalate ", " names
    placeholders = intercalate ", " (map (const "?") names)

lastContentRowInsertIdQuery :: Query
lastContentRowInsertIdQuery =
  "SELECT seq FROM sqlite_sequence WHERE name=\"" <> tableName <> "\""

data ContentRow = ContentRow
  { crId             :: Maybe Integer
  , crHashId         :: ContentId
  , crUrl            :: ContentURI
  , crState          :: ContentState
  , crInitializedAt  :: Maybe UTCTime
  , crActiveAt       :: Maybe UTCTime
  , crCompletedAt    :: Maybe UTCTime
  , crMime           :: Maybe String
  , crSize           :: Maybe Integer
  , crProgress       :: Float
  , crDziWidth       :: Maybe Integer
  , crDziHeight      :: Maybe Integer
  , crDziTileSize    :: Maybe TileSize
  , crDziTileOverlap :: Maybe TileOverlap
  , crDziTileFormat  :: Maybe TileFormat
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
    , toField crUrl
    , toField crState
    -- , toField crInitializedAt -- Omitted due to SQLite default value
    , toField crActiveAt
    , toField crCompletedAt
    , toField crMime
    , toField crSize
    , toField crProgress
    , toField crDziWidth
    , toField crDziHeight
    , toField crDziTileSize
    , toField crDziTileOverlap
    , toField crDziTileFormat
    ]

rowToContent :: ContentRow -> Content
rowToContent cr = Content
    { contentId = crHashId cr
    , contentUrl = crUrl cr
    , contentState = crState cr
    , contentInitializedAt = crInitializedAt cr
    , contentActiveAt = crActiveAt cr
    , contentCompletedAt = crCompletedAt cr
    , contentMime = crMime cr
    , contentSize = crSize cr
    , contentProgress = crProgress cr
    , contentDzi = maybeDZI
    }
  where
    maybeDZIWidth = crDziWidth cr
    maybeDZIHeight = crDziHeight cr
    maybeDZITileSize = crDziTileSize cr
    maybeDZITileOverlap = crDziTileOverlap cr
    maybeDZITileFormat = crDziTileFormat cr
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
    , crUrl = contentUrl c
    , crState = contentState c
    , crInitializedAt = contentInitializedAt c
    , crActiveAt = contentActiveAt c
    , crCompletedAt = contentCompletedAt c
    , crMime = contentMime c
    , crSize = contentSize c
    , crProgress = contentProgress c
    , crDziWidth = dziWidth <$> dzi
    , crDziHeight = dziHeight <$> dzi
    , crDziTileSize = dziTileSize <$> dzi
    , crDziTileOverlap = dziTileOverlap  <$> dzi
    , crDziTileFormat = dziTileFormat  <$> dzi
    }
  where dzi = contentDzi c
