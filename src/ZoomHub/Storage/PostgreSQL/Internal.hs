{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- Simplify Squeal query type signatures
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module ZoomHub.Storage.PostgreSQL.Internal
  ( module ZoomHub.Storage.PostgreSQL.Internal
  , destroyAllResources
  ) where

import ZoomHub.Storage.PostgreSQL.Schema (Schema)
import ZoomHub.Types.Content.Internal (Content(..))
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentState (ContentState(..))
import qualified ZoomHub.Types.ContentState as ContentState
import ZoomHub.Types.ContentType (ContentType(..))
import qualified ZoomHub.Types.ContentType as ContentType
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.DeepZoomImage (DeepZoomImage(..), mkDeepZoomImage)
import ZoomHub.Types.DeepZoomImage.TileFormat (TileFormat)
import ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap)
import ZoomHub.Types.DeepZoomImage.TileSize (TileSize)

import Control.Monad (void, when)
import Control.Monad.Base (liftBase)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Char8 as BC
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time (NominalDiffTime)
import Data.Time.Units (Second, TimeUnit, toMicroseconds)
import Database.PostgreSQL.Simple (ConnectInfo(..))
import qualified Generics.SOP as SOP
import Generics.SOP.BasicFunctors (K)
import qualified GHC.Generics as GHC
import Squeal.PostgreSQL
  ( ColumnValue(Default, Same, Set)
  , Condition
  , ConflictClause(OnConflictDoRaise)
  , Expression
  , Grouping(Ungrouped)
  , Manipulation
  , MonadPQ
  , NP((:*))
  , NullityType(NotNull, Null)
  , Only(..)
  , PGType(PGint8, PGtext)
  , Query
  , ReturningClause(Returning)
  , RowPG
  , TableExpression
  , ToParam
  , TuplePG
  , as
  , currentTimestamp
  , firstRow
  , from
  , insertQuery_
  , insertRow
  , leftOuterJoin
  , manipulateParams
  , null_
  , param
  , runQueryParams
  , select
  , table
  , transactionally_
  , update
  , update_
  , where_
  , (!)
  , (&)
  , (.==)
  )
import Squeal.PostgreSQL.Pool (Pool, destroyAllResources)
import qualified Squeal.PostgreSQL.Pool as P
import qualified Squeal.PostgreSQL.PQ as PQ
import System.Random (randomRIO)

-- Connection
type Connection = K PQ.Connection Schema

createConnectionPool ::
  (TimeUnit a, MonadBase IO io) =>
  ConnectInfo -> Integer -> a -> Integer -> io (Pool Connection)
createConnectionPool connInfo numStripes idleTime maxResourcesPerStripe =
  P.createConnectionPool (connectionString connInfo)
    (fromIntegral numStripes) (toNominalDiffTime idleTime)
    (fromIntegral maxResourcesPerStripe)

  where
    connectionString :: ConnectInfo -> BC.ByteString
    connectionString info = BC.intercalate " "
      [ pair "host" (connectHost info)
      , pair "port" (show $ connectPort info)
      , pair "user" (connectUser info)
      , pair "password" (connectPassword info)
      , pair "dbname" (connectDatabase info)
      ]

    pair :: BC.ByteString -> String -> BC.ByteString
    pair key value = key <> "=" <> BC.pack value

-- Reads: Content
getBy ::
  (MonadBaseControl IO m, MonadPQ Schema m, ToParam p a) =>
  Condition Schema _ 'Ungrouped '[ 'NotNull a ] ->
  p ->
  m (Maybe Content)
getBy condition parameter = do
  result <- runQueryParams
    (selectContentBy (\t -> t & where_ condition)) (Only parameter)
  contentRow <- firstRow result
  pure (contentImageRowToContent <$> contentRow)

getBy' ::
  (MonadBaseControl IO m, MonadPQ Schema m, ToParam p 'PGtext) =>
  Condition Schema _ 'Ungrouped '[ 'NotNull 'PGtext ] ->
  p ->
  m (Maybe Content)
getBy' condition parameter = do
    mContent <- getBy condition parameter
    case mContent of
      Just content -> do
        -- Sample how often we count views to reduce database load:
        -- http://stackoverflow.com/a/4762559/125305
        let cId = contentId content
        let numViews = contentNumViews content
        let numViewsSampleRate = sampleRate numViews
        numViewsSample <- liftBase $ randomRIO (1, numViewsSampleRate)
        when (numViewsSample == 1) $
          -- TODO: How can we run this async?
          void $ manipulateParams incrNumViews (numViewsSampleRate, cId)
        return $ Just content
      Nothing ->
        return Nothing
  where
    sampleRate :: Int64 -> Int64
    sampleRate numViews
      | numViews < 100 = 1
      | numViews < 1000 = 10
      | numViews < 10000 = 100
      | otherwise = 1000

incrNumViews :: Manipulation Schema '[ 'NotNull 'PGint8, 'NotNull 'PGtext ] '[]
incrNumViews =
  update_ #content
    ( Same `as` #id :*
      Same `as` #hash_id :*
      Same `as` #type_id :*
      Same `as` #url :*
      Same `as` #state :*
      Same `as` #initialized_at :*
      Same `as` #active_at :*
      Same `as` #completed_at :*
      Same `as` #title :*
      Same `as` #attribution_text :*
      Same `as` #attribution_link :*
      Same `as` #mime :*
      Same `as` #size :*
      Same `as` #error :*
      Same `as` #progress :*
      Same `as` #abuse_level_id :*
      Same `as` #num_abuse_reports :*
      Set ( #num_views + param @1 ) `as` #num_views :*
      Same `as` #version
    )
    ( #hash_id .== param @2 )

selectContentBy
  :: ( TableExpression Schema '[ 'NotNull a ] _ 'Ungrouped ->
       TableExpression Schema '[ 'NotNull a ] _ 'Ungrouped
     )
  -> Query Schema '[ 'NotNull a ] (RowPG ContentImageRow)
selectContentBy clauses = select
  ( #content ! #hash_id `as` #cirHashId :*
    #content ! #type_id `as` #cirTypeId :*
    #content ! #url `as` #cirURL :*
    #content ! #state `as` #cirState :*
    #content ! #initialized_at `as` #cirInitializedAt :*
    #content ! #active_at `as` #cirActiveAt :*
    #content ! #completed_at `as` #cirCompletedAt :*
    #content ! #title `as` #cirTitle :*
    #content ! #attribution_text `as` #cirAttributionText :*
    #content ! #attribution_link `as` #cirAttributionLink :*
    #content ! #mime `as` #cirMIME :*
    #content ! #size `as` #cirSize :*
    #content ! #error `as` #cirError :*
    #content ! #progress `as` #cirProgress :*
    #content ! #abuse_level_id `as` #cirAbuseLevelId :*
    #content ! #num_abuse_reports `as` #cirNumAbuseReports :*
    #content ! #num_views `as` #cirNumViews :*
    #content ! #version `as` #cirVersion :*
    #image ! #width `as` #cirWidth :*
    #image ! #height `as` #cirHeight :*
    #image ! #tile_size `as` #cirTileSize :*
    #image ! #tile_overlap `as` #cirTileOverlap :*
    #image ! #tile_format `as` #cirTileFormat
  )
  ( from ( table #content
         & leftOuterJoin (table #image) (#content ! #id .== #image ! #content_id)
         )
  & clauses
  )

-- Reads: Image
getImageById :: (MonadBaseControl IO m, MonadPQ Schema m) => Int64 -> m (Maybe DeepZoomImage)
getImageById cid = do
  result <- runQueryParams (selectImageBy ((#image ! #content_id) .== param @1)) (Only cid)
  imageRow <- firstRow result
  pure (imageRowToImage <$> imageRow)

selectImageBy ::
  Condition Schema _ 'Ungrouped '[ 'NotNull 'PGint8 ] ->
  Query Schema '[ 'NotNull 'PGint8 ] (RowPG ImageRow)
selectImageBy condition = select
  ( #image ! #created_at `as` #irCreatedAt :*
    #image ! #width `as` #irWidth :*
    #image ! #height `as` #irHeight :*
    #image ! #tile_size `as` #irTileSize :*
    #image ! #tile_overlap `as` #irTileOverlap :*
    #image ! #tile_format `as` #irTileFormat
  )
  ( from (table #image) & where_ condition )

-- Writes: Image
-- createImage :: (MonadBaseControl IO m, MonadPQ Schema m) => Int64 -> UTCTime -> DeepZoomImage -> m Int64
-- createImage cid initializedAt image = do
--   let imageRow = imageToRow cid image initializedAt
--   result <- manipulateParams insertImage imageRow
--   fmap fromOnly . getRow 0 $ result

-- TODO: How can we avoid this duplication between `ContentRow`,
-- `ContentImageRow`, and `ImageRow`?
data ContentImageRow = ContentImageRow
  { -- content
    cirHashId :: ContentId
  , cirTypeId :: ContentType
  , cirURL :: ContentURI
  , cirState :: ContentState
  , cirInitializedAt :: UTCTime
  , cirActiveAt :: Maybe UTCTime
  , cirCompletedAt :: Maybe UTCTime
  , cirTitle :: Maybe Text
  , cirAttributionText :: Maybe Text
  , cirAttributionLink :: Maybe Text
  , cirMIME :: Maybe ContentMIME
  , cirSize :: Maybe Int64
  , cirError :: Maybe Text
  , cirProgress :: Double
  , cirAbuseLevelId :: Int32
  , cirNumAbuseReports :: Int64
  , cirNumViews :: Int64
  , cirVersion :: Int32
    -- image
  , cirWidth :: Maybe Int64
  , cirHeight :: Maybe Int64
  , cirTileSize :: Maybe TileSize
  , cirTileOverlap :: Maybe TileOverlap
  , cirTileFormat :: Maybe TileFormat
  } deriving (Show, GHC.Generic)
instance SOP.Generic ContentImageRow
instance SOP.HasDatatypeInfo ContentImageRow

data ImageRow = ImageRow
  { irCreatedAt :: UTCTime
  , irWidth :: Int64
  , irHeight :: Int64
  , irTileSize :: TileSize
  , irTileOverlap :: TileOverlap
  , irTileFormat :: TileFormat
  } deriving (Show, GHC.Generic)
instance SOP.Generic ImageRow
instance SOP.HasDatatypeInfo ImageRow

contentImageRowToContent :: ContentImageRow -> Content
contentImageRowToContent cr = Content
    { contentId = cirHashId cr
    , contentType = cirTypeId cr
    , contentURL = cirURL cr
    , contentState = cirState cr
    , contentInitializedAt = cirInitializedAt cr
    , contentActiveAt = cirActiveAt cr
    , contentCompletedAt = cirCompletedAt cr
    , contentMIME = cirMIME cr
    , contentSize = fromIntegral <$> cirSize cr
    , contentProgress = cirProgress cr
    , contentNumViews = fromIntegral (cirNumViews cr)
    , contentError = cirError cr
    , contentDZI = mDZI
    }
  where
    mDZI = mkDeepZoomImage <$>
      (fromIntegral <$> cirWidth cr) <*>
      (fromIntegral <$> cirHeight cr) <*>
      cirTileSize cr <*>
      cirTileOverlap cr <*>
      cirTileFormat cr

imageRowToImage :: ImageRow -> DeepZoomImage
imageRowToImage ir = mkDeepZoomImage
  (fromIntegral . irWidth $ ir)
  (fromIntegral . irHeight $ ir)
  (irTileSize ir)
  (irTileOverlap ir)
  (irTileFormat ir)

contentRowToContent :: ContentRow -> Content
contentRowToContent result =
  Content
  { contentId = crHashId result
  , contentType = crTypeId result
  , contentURL = crURL result
  , contentState = crState result
  , contentInitializedAt = crInitializedAt result
  , contentActiveAt = crActiveAt result
  , contentCompletedAt = crCompletedAt result
  , contentMIME = crMIME result
  , contentSize = crSize result
  , contentProgress = crProgress result
  , contentNumViews = crNumViews result
  , contentError = crError result
  , contentDZI = Nothing
  }

data ContentRow =
  ContentRow
  { crHashId :: ContentId           -- 1
  , crTypeId :: ContentType         -- 2
  , crURL :: ContentURI             -- 3
  , crState :: ContentState         -- 4
  , crInitializedAt :: UTCTime      -- 5
  , crActiveAt :: Maybe UTCTime     -- 6
  , crCompletedAt :: Maybe UTCTime  -- 7
  , crTitle :: Maybe Text           -- 8
  , crAttributionText :: Maybe Text -- 9
  , crAttributionLink :: Maybe Text -- 10
  , crMIME :: Maybe ContentMIME     -- 11
  , crSize :: Maybe Int64           -- 12
  , crError :: Maybe Text           -- 13
  , crProgress :: Double            -- 14
  , crAbuseLevelId :: Int32         -- 15
  , crNumAbuseReports :: Int64      -- 16
  , crNumViews :: Int64             -- 17
  , crVersion :: Int32              -- 18
  } deriving (Show, GHC.Generic)
instance SOP.Generic ContentRow
instance SOP.HasDatatypeInfo ContentRow

insertContent :: Manipulation Schema '[ 'NotNull 'PGtext ] (RowPG ContentRow)
insertContent = insertRow #content
  ( Default `as` #id :*
    Set unsafeHashIdPlaceholder `as` #hash_id :*
    Default `as` #type_id :*
    Set (param @1) `as` #url :*
    Default `as` #state :*
    Default `as` #initialized_at :*
    Default `as` #active_at :*
    Default `as` #completed_at :*
    Default `as` #title :*
    Default `as` #attribution_text :*
    Default `as` #attribution_link :*
    Default `as` #mime :*
    Default `as` #size :*
    Default `as` #error :*
    Default `as` #progress :*
    Default `as` #abuse_level_id :*
    Default `as` #num_abuse_reports :*
    Default `as` #num_views :*
    Default `as` #version
  ) OnConflictDoRaise
  ( Returning
    ( #hash_id `as` #crHashId :*
      #type_id `as` #crTypeId :*
      #url `as` #crURL :*
      #state `as` #crState :*
      #initialized_at `as` #crInitializedAt :*
      #active_at `as` #crActiveAt :*
      #completed_at `as` #crCompletedAt :*
      #title `as` #crTitle :*
      #attribution_text `as` #crAttributionText :*
      #attribution_link `as` #crAttributionLink :*
      #mime `as` #crMIME :*
      #size `as` #crSize :*
      #error `as` #crError :*
      #progress `as` #crProgress :*
      #abuse_level_id `as` #crAbuseLevelId :*
      #num_abuse_reports `as` #crNumAbuseReports :*
      #num_views `as` #crNumViews :*
      #version `as` #crVersion
    )
  )
  where
    unsafeHashIdPlaceholder :: Expression schema from grouping params (nullity 'PGtext)
    unsafeHashIdPlaceholder = "$placeholder-overwritten-by-trigger$"

markContentAsActive ::
  Manipulation Schema
  '[ 'NotNull 'PGtext ]
  (RowPG ContentRow)
markContentAsActive = update #content
  ( Same `as` #id :*
    Same `as` #hash_id :*
    Set (ContentType.toExpression Unknown) `as` #type_id :*
    Same `as` #url :*
    Set (ContentState.toExpression Active) `as` #state :*
    Same `as` #initialized_at :*
    Set currentTimestamp `as` #active_at :*
    Set null_ `as` #completed_at :*
    Set null_ `as` #title :*
    Set null_ `as` #attribution_text :*
    Set null_ `as` #attribution_link :*
    Set null_ `as` #mime :*
    Set null_ `as` #size :*
    Set null_ `as` #error :*
    Set 0.0 `as` #progress :*
    Same `as` #abuse_level_id :*
    Same `as` #num_abuse_reports :*
    Same `as` #num_views :*
    Same `as` #version
  )
  ( #hash_id .== param @1 )
  ( Returning
    ( #hash_id `as` #crHashId :*
      #type_id `as` #crTypeId :*
      #url `as` #crURL :*
      #state `as` #crState :*
      #initialized_at `as` #crInitializedAt :*
      #active_at `as` #crActiveAt :*
      #completed_at `as` #crCompletedAt :*
      #title `as` #crTitle :*
      #attribution_text `as` #crAttributionText :*
      #attribution_link `as` #crAttributionLink :*
      #mime `as` #crMIME :*
      #size `as` #crSize :*
      #error `as` #crError :*
      #progress `as` #crProgress :*
      #abuse_level_id `as` #crAbuseLevelId :*
      #num_abuse_reports `as` #crNumAbuseReports :*
      #num_views `as` #crNumViews :*
      #version `as` #crVersion
    )
  )

markContentAsFailure ::
  Manipulation Schema
  '[ 'NotNull 'PGtext, 'Null 'PGtext ]
  (RowPG ContentRow)
markContentAsFailure = update #content
  ( Same `as` #id :*
    Same `as` #hash_id :*
    Set (ContentType.toExpression Unknown) `as` #type_id :*
    Same `as` #url :*
    Set (ContentState.toExpression CompletedFailure) `as` #state :*
    Same `as` #initialized_at :*
    Same `as` #active_at :*
    Set currentTimestamp `as` #completed_at :*
    Set null_ `as` #title :*
    Set null_ `as` #attribution_text :*
    Set null_ `as` #attribution_link :*
    Set null_ `as` #mime :*
    Set null_ `as` #size :*
    Set (param @2) `as` #error :*
    Set 1.0 `as` #progress :*
    Same `as` #abuse_level_id :*
    Same `as` #num_abuse_reports :*
    Same `as` #num_views :*
    Same `as` #version
  )
  ( #hash_id .== param @1 )
  ( Returning
    ( #hash_id `as` #crHashId :*
      #type_id `as` #crTypeId :*
      #url `as` #crURL :*
      #state `as` #crState :*
      #initialized_at `as` #crInitializedAt :*
      #active_at `as` #crActiveAt :*
      #completed_at `as` #crCompletedAt :*
      #title `as` #crTitle :*
      #attribution_text `as` #crAttributionText :*
      #attribution_link `as` #crAttributionLink :*
      #mime `as` #crMIME :*
      #size `as` #crSize :*
      #error `as` #crError :*
      #progress `as` #crProgress :*
      #abuse_level_id `as` #crAbuseLevelId :*
      #num_abuse_reports `as` #crNumAbuseReports :*
      #num_views `as` #crNumViews :*
      #version `as` #crVersion
    )
  )

markContentAsSuccess ::
  Manipulation Schema
  '[ 'NotNull 'PGtext, 'Null 'PGtext, 'Null 'PGint8 ]
  (RowPG ContentRow)
markContentAsSuccess = update #content
  ( Same `as` #id :*
    Same `as` #hash_id :*
    Set (ContentType.toExpression Image) `as` #type_id :*
    Same `as` #url :*
    Set (ContentState.toExpression CompletedSuccess) `as` #state :*
    Same `as` #initialized_at :*
    Same `as` #active_at :*
    Set currentTimestamp `as` #completed_at :*
    Same `as` #title :*
    Same `as` #attribution_text :*
    Same `as` #attribution_link :*
    Set (param @2) `as` #mime :*
    Set (param @3) `as` #size :*
    Set null_ `as` #error :* -- reset any previous errors
    Set 1.0 `as` #progress :*
    Same `as` #abuse_level_id :*
    Same `as` #num_abuse_reports :*
    Same `as` #num_views :*
    Same `as` #version
  )
  ( #hash_id .== param @1 )
  ( Returning
    ( #hash_id `as` #crHashId :*
      #type_id `as` #crTypeId :*
      #url `as` #crURL :*
      #state `as` #crState :*
      #initialized_at `as` #crInitializedAt :*
      #active_at `as` #crActiveAt :*
      #completed_at `as` #crCompletedAt :*
      #title `as` #crTitle :*
      #attribution_text `as` #crAttributionText :*
      #attribution_link `as` #crAttributionLink :*
      #mime `as` #crMIME :*
      #size `as` #crSize :*
      #error `as` #crError :*
      #progress `as` #crProgress :*
      #abuse_level_id `as` #crAbuseLevelId :*
      #num_abuse_reports `as` #crNumAbuseReports :*
      #num_views `as` #crNumViews :*
      #version `as` #crVersion
    )
  )

resetContentAsInitialized ::
  Manipulation Schema
  '[ 'NotNull 'PGtext ]
  (RowPG ContentRow)
resetContentAsInitialized = update #content
  ( Same `as` #id :*
    Same `as` #hash_id :*
    Set (ContentType.toExpression Unknown) `as` #type_id :*
    Same `as` #url :*
    Set (ContentState.toExpression Initialized) `as` #state :*
    Same `as` #initialized_at :*
    Set null_ `as` #active_at :*
    Set null_ `as` #completed_at :*
    Same `as` #title :*
    Same `as` #attribution_text :*
    Same `as` #attribution_link :*
    Set null_ `as` #mime :*
    Set null_ `as` #size :*
    Set null_ `as` #error :* -- reset any previous errors
    Set 0.0 `as` #progress :*
    Same `as` #abuse_level_id :*
    Same `as` #num_abuse_reports :*
    Same `as` #num_views :*
    Same `as` #version
  )
  ( #hash_id .== param @1 )
  ( Returning
    ( #hash_id `as` #crHashId :*
      #type_id `as` #crTypeId :*
      #url `as` #crURL :*
      #state `as` #crState :*
      #initialized_at `as` #crInitializedAt :*
      #active_at `as` #crActiveAt :*
      #completed_at `as` #crCompletedAt :*
      #title `as` #crTitle :*
      #attribution_text `as` #crAttributionText :*
      #attribution_link `as` #crAttributionLink :*
      #mime `as` #crMIME :*
      #size `as` #crSize :*
      #error `as` #crError :*
      #progress `as` #crProgress :*
      #abuse_level_id `as` #crAbuseLevelId :*
      #num_abuse_reports `as` #crNumAbuseReports :*
      #num_views `as` #crNumViews :*
      #version `as` #crVersion
    )
  )

imageToInsertRow :: ContentId -> DeepZoomImage -> InsertImageRow
imageToInsertRow cid dzi = InsertImageRow
  { iirContentId = cid
  , iirWidth = fromIntegral . dziWidth $ dzi
  , iirHeight = fromIntegral . dziHeight $ dzi
  , iirTileSize = dziTileSize dzi
  , iirTileOverlap = dziTileOverlap dzi
  , iirTileFormat = dziTileFormat dzi
  }

data InsertImageRow = InsertImageRow
  { iirContentId :: ContentId
  , iirWidth :: Int64
  , iirHeight :: Int64
  , iirTileSize :: TileSize
  , iirTileOverlap :: TileOverlap
  , iirTileFormat :: TileFormat
  } deriving (Show, GHC.Generic)
instance SOP.Generic InsertImageRow
instance SOP.HasDatatypeInfo InsertImageRow

insertImage :: Manipulation Schema (TuplePG InsertImageRow) '[]
insertImage = insertQuery_ #image $
  select
    ( #content ! #id `as` #content_id :*
      currentTimestamp `as` #created_at :*
      param @2 `as` #width :*
      param @3 `as` #height :*
      param @4 `as` #tile_size :*
      param @5 `as` #tile_overlap :*
      param @6 `as` #tile_format
    )
    ( from (table #content)
    & where_ (#content ! #hash_id .== param @1)
    )

-- Unsafe
unsafeCreateContent
  :: (MonadBaseControl IO m, MonadPQ Schema m)
  => Content
  -> m (Maybe Content)
unsafeCreateContent content =
  transactionally_ $ do
    result <- manipulateParams unsafeInsertContent (contentToRow content)
    contentRow <- firstRow result
    pure $ contentRowToContent <$> contentRow

unsafeInsertContent :: Manipulation Schema (TuplePG ContentRow) (RowPG ContentRow)
unsafeInsertContent = insertRow #content
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
  ) OnConflictDoRaise
  ( Returning
    ( #hash_id `as` #crHashId :*
      #type_id `as` #crTypeId :*
      #url `as` #crURL :*
      #state `as` #crState :*
      #initialized_at `as` #crInitializedAt :*
      #active_at `as` #crActiveAt :*
      #completed_at `as` #crCompletedAt :*
      #title `as` #crTitle :*
      #attribution_text `as` #crAttributionText :*
      #attribution_link `as` #crAttributionLink :*
      #mime `as` #crMIME :*
      #size `as` #crSize :*
      #error `as` #crError :*
      #progress `as` #crProgress :*
      #abuse_level_id `as` #crAbuseLevelId :*
      #num_abuse_reports `as` #crNumAbuseReports :*
      #num_views `as` #crNumViews :*
      #version `as` #crVersion
    )
  )

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
  , crAbuseLevelId = 0 -- TODO: Replace hard-coded value
  , crNumAbuseReports = 0
  , crNumViews = contentNumViews c
  , crVersion = 4 -- TODO: Replace hard-coded value
  }

toNominalDiffTime :: TimeUnit a => a -> NominalDiffTime
toNominalDiffTime duration = fromIntegral $
  toMicroseconds duration `div` toMicroseconds (1 :: Second)
