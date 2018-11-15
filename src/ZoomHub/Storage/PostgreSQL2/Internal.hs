{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- Simplify Squeal query type signatures
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module ZoomHub.Storage.PostgreSQL2.Internal where

import ZoomHub.Storage.PostgreSQL.Schema (Schema)
import ZoomHub.Types.Content (Content(..))
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentState (ContentState)
import ZoomHub.Types.ContentType (ContentType)
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.DeepZoomImage (DeepZoomImage(..), mkDeepZoomImage)
import ZoomHub.Types.DeepZoomImage.TileFormat (TileFormat)
import ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap)
import ZoomHub.Types.DeepZoomImage.TileSize (TileSize)

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL
  ( (:::)
  , ColumnValue(Default, Set)
  , Condition
  , ConflictClause(OnConflictDoRaise)
  , Grouping(Ungrouped)
  , Manipulation
  , MonadPQ
  , NP((:*))
  -- , NullityType(NotNull, Null)
  , NullityType(NotNull)
  , Only(..)
  -- , PGType(PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz)
  , PGType(PGint8, PGtext)
  , Query
  , ReturningClause(Returning)
  , RowPG
  , TuplePG
  , (!)
  , (&)
  , (.==)
  , as
  , from
  , fromOnly
  , firstRow
  , getRow
  , insertRow
--   , leftOuterJoin
  , manipulateParams
  , param
  , runQueryParams
  , select
  , table
  , where_
  )

-- Public API

-- Reads
createImage :: (MonadBaseControl IO m, MonadPQ Schema m) => Int64 -> UTCTime -> DeepZoomImage -> m Int64
createImage cid initializedAt image = do
  let imageRow = imageToRow cid image initializedAt
  result <- manipulateParams insertImage imageRow
  fmap fromOnly . getRow 0 $ result

getImageById :: (MonadBaseControl IO m, MonadPQ Schema m) => Int64 -> m (Maybe DeepZoomImage)
getImageById cid = do
  result <- runQueryParams (selectImageBy ((#image ! #content_id) .== param @1)) (Only cid)
  imageRow <- firstRow result
  pure (rowToImage <$> imageRow)

selectImageBy ::
  Condition Schema _ 'Ungrouped '[ 'NotNull 'PGint8 ] ->
  Query
  Schema
  '[ 'NotNull 'PGint8 ]
  (RowPG ImageRow)
selectImageBy condition = select
  ( #image ! #content_id `as` #irContentId :*
    #image ! #initialized_at `as` #irInitializedAt :*
    #image ! #width `as` #irWidth :*
    #image ! #height `as` #irHeight :*
    #image ! #tile_size `as` #irTileSize :*
    #image ! #tile_overlap `as` #irTileOverlap :*
    #image ! #tile_format `as` #irTileFormat
  )
  ( from (table #image) & where_ condition )

selectContentBy ::
  Condition Schema _ 'Ungrouped '[ 'NotNull 'PGtext ] ->
  Query
  Schema
  '[ 'NotNull 'PGtext ]
  (RowPG ContentRow)
selectContentBy condition = select
  ( #content ! #hash_id `as` #crHashId :*
    #content ! #type_id `as` #crTypeId :*
    #content ! #url `as` #crURL :*
    #content ! #state `as` #crState :*
    #content ! #initialized_at `as` #crInitializedAt :*
    #content ! #active_at `as` #crActiveAt :*
    #content ! #completed_at `as` #crCompletedAt :*
    #content ! #title `as` #crTitle :*
    #content ! #attribution_text `as` #crAttributionText :*
    #content ! #attribution_link `as` #crAttributionLink :*
    #content ! #mime `as` #crMIME :*
    #content ! #size `as` #crSize :*
    #content ! #error `as` #crError :*
    #content ! #progress `as` #crProgress :*
    #content ! #abuse_level_id `as` #crAbuseLevelId :*
    #content ! #num_abuse_reports `as` #crNumAbuseReports :*
    #content ! #num_views `as` #crNumViews :*
    #content ! #version `as` #crVersion
    -- #image ! #width `as` #irWidth :*
    -- #image ! #height `as` #irHeight :*
    -- #image ! #tile_size `as` #irTileSize :*
    -- #image ! #tile_overlap `as` #irTileOverlap :*
    -- #image ! #tile_format `as` #irTileFormat
  )
  ( from (table #content
        --  & leftOuterJoin (table #image) (#content ! #id .== #image ! #content_id)
         )
    & where_ condition
  )

data ContentRow = ContentRow
  {{- crId :: Int64
  , -}crHashId :: ContentId
  , crTypeId :: ContentType
  , crURL :: ContentURI
  , crState :: ContentState
  , crInitializedAt :: UTCTime
  , crActiveAt :: Maybe UTCTime
  , crCompletedAt :: Maybe UTCTime
  , crTitle :: Maybe Text
  , crAttributionText :: Maybe Text
  , crAttributionLink :: Maybe Text
  , crMIME :: Maybe ContentMIME
  , crSize :: Maybe Int64
  , crError :: Maybe Text
  , crProgress :: Double
  , crAbuseLevelId :: Int32
  , crNumAbuseReports :: Int64
  , crNumViews :: Int64
  , crVersion :: Int32
  -- , irWidth :: Maybe Int64
  -- , irHeight :: Maybe Int64
  -- , irTileSize :: Maybe TileSize
  -- , irTileOverlap :: Maybe TileOverlap
  -- , irTileFormat :: Maybe TileFormat
  } deriving (Show, GHC.Generic)
instance SOP.Generic ContentRow
instance SOP.HasDatatypeInfo ContentRow

data ImageRow = ImageRow
  { irContentId :: Int64
  , irInitializedAt :: UTCTime
  , irWidth :: Int64
  , irHeight :: Int64
  , irTileSize :: TileSize
  , irTileOverlap :: TileOverlap
  , irTileFormat :: TileFormat
  } deriving (Show, GHC.Generic)
instance SOP.Generic ImageRow
instance SOP.HasDatatypeInfo ImageRow

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
    , contentSize = fromIntegral <$> crSize cr
    , contentProgress = crProgress cr
    , contentNumViews = fromIntegral (crNumViews cr)
    , contentError = crError cr
    , contentDZI = Nothing
    }
  -- where
  --   mDZI = mkDeepZoomImage <$>
  --     (fromIntegral <$> irWidth cr) <*>
  --     (fromIntegral <$> irHeight cr) <*>
  --     irTileSize cr <*>
  --     irTileOverlap cr <*>
  --     irTileFormat cr

rowToImage :: ImageRow -> DeepZoomImage
rowToImage ir = mkDeepZoomImage
  (fromIntegral . irWidth $ ir)
  (fromIntegral . irHeight $ ir)
  (irTileSize ir)
  (irTileOverlap ir)
  (irTileFormat ir)

contentToRow :: Content -> ContentRow
contentToRow c = ContentRow
  { crHashId = contentId c
  , crTypeId = contentType c
  , crURL = contentURL c
  , crState = contentState c
  , crInitializedAt = contentInitializedAt c
  , crActiveAt = contentActiveAt c
  , crCompletedAt = contentCompletedAt c
  , crTitle = Nothing
  , crAttributionText = Nothing
  , crAttributionLink = Nothing
  , crMIME = contentMIME c
  , crSize = contentSize c
  , crError = contentError c
  , crProgress = contentProgress c
  , crAbuseLevelId = 0
  , crNumAbuseReports = 0
  , crNumViews = contentNumViews c
  , crVersion = 4
  -- , irWidth = (fromIntegral . dziWidth) <$> dzi
  -- , irHeight = (fromIntegral . dziHeight) <$> dzi
  -- , irTileSize = dziTileSize <$> dzi
  -- , irTileOverlap = dziTileOverlap <$> dzi
  -- , irTileFormat = dziTileFormat <$> dzi
  }
-- where
--   dzi = contentDZI c

imageToRow :: Int64 -> DeepZoomImage -> UTCTime -> ImageRow
imageToRow cid dzi initializedAt = ImageRow
  { irContentId = cid
  , irInitializedAt = initializedAt
  , irWidth = fromIntegral . dziWidth $ dzi
  , irHeight = fromIntegral . dziHeight $ dzi
  , irTileSize = dziTileSize dzi
  , irTileOverlap = dziTileOverlap dzi
  , irTileFormat = dziTileFormat dzi
  }

insertContent :: Manipulation Schema (TuplePG ContentRow) '[ "fromOnly" ::: 'NotNull 'PGtext ]
insertContent = insertRow #content
  ( Default `as` #id :*
    Set (param @1) `as` #hash_id :*
    Set (param @2) `as` #type_id :*
    Set (param @3) `as` #url :*
    Set (param @4) `as` #state :*
    Set (param @5) `as` #initialized_at :*
    Set (param @6) `as` #active_at :*
    Set (param @7) `as` #completed_at :*
    Set (param @8) `as` #title :*
    Set (param @9) `as` #attribution_text :*
    Set (param @10) `as` #attribution_link :*
    Set (param @11) `as` #mime :*
    Set (param @12) `as` #size :*
    Set (param @13) `as` #error :*
    Set (param @14) `as` #progress :*
    Set (param @15) `as` #abuse_level_id :*
    Set (param @16) `as` #num_abuse_reports :*
    Set (param @17) `as` #num_views :*
    Set (param @18) `as` #version
  ) OnConflictDoRaise (Returning (#hash_id `as` #fromOnly))

insertImage :: Manipulation Schema (TuplePG ImageRow) '[ "fromOnly" ::: 'NotNull 'PGint8 ]
insertImage = insertRow #image
  ( Set (param @1) `as` #content_id :*
    Set (param @2) `as` #initialized_at :*
    Set (param @3) `as` #width :*
    Set (param @4) `as` #height :*
    Set (param @5) `as` #tile_size :*
    Set (param @6) `as` #tile_overlap :*
    Set (param @7) `as` #tile_format
  ) OnConflictDoRaise (Returning (#content_id `as` #fromOnly))
