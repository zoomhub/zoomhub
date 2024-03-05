{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- Simplify Squeal query type signatures
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# HLINT ignore "Use <$>" #-}


module ZoomHub.Storage.PostgreSQL.Internal
  ( module ZoomHub.Storage.PostgreSQL.Internal,
    destroyConnectionPool,
    usingConnectionPool,
  )
where

import Control.Monad (when)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Units (Second, TimeUnit, toMicroseconds)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Generics.SOP.BasicFunctors (K)
import Squeal.PostgreSQL
  ( Condition,
    ConflictClause (OnConflictDoRaise),
    DecodeRow,
    GenericParams (genericParams),
    GenericRow (genericRow),
    Grouping (Ungrouped),
    Manipulation_,
    MonadPQ (execute, executeParams),
    MonadResult (getRows),
    NP (Nil, (:*)),
    NullType (NotNull, Null),
    Only (..),
    Optional (Default, Set),
    PGType (PGint4, PGint8, PGtext, PGtimestamptz, PGfloat8),
    PQ,
    Query,
    QueryClause (Subquery),
    Query_,
    RowPG,
    Selection (Star),
    Statement (Query),
    TableExpression,
    ToPG,
    UsingClause (Using),
    as,
    currentTimestamp,
    deleteFrom,
    firstRow,
    from,
    inline,
    insertInto,
    insertInto_,
    leftOuterJoin,
    manipulateParams,
    manipulateParams_,
    null_,
    param,
    query,
    runQueryParams,
    select,
    select_,
    table,
    transactionally_,
    update,
    update_,
    where_,
    (!),
    (&),
    (.&&),
    (.==),
    (:::),
    appendRows,
    Join,
  )
import Squeal.PostgreSQL.Manipulation (pattern Returning_)
import Squeal.PostgreSQL.Manipulation.Insert (pattern Values_)
import qualified Squeal.PostgreSQL.Session.Connection as Connection
import Squeal.PostgreSQL.Session.Pool (destroyConnectionPool, usingConnectionPool)
import qualified Squeal.PostgreSQL.Session.Pool as P
import System.Random (randomRIO)
import UnliftIO (MonadUnliftIO (..), liftIO)
import qualified ZoomHub.Storage.PostgreSQL.ConnectInfo as ConnectInfo
import ZoomHub.Storage.PostgreSQL.Schema (Schemas)
import qualified ZoomHub.Types.Content as Content
import ZoomHub.Types.Content.Internal (Content (..))
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentState (ContentState (..))
import ZoomHub.Types.ContentType (ContentType (..))
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.DeepZoomImage (DeepZoomImage (..), mkDeepZoomImage)
import ZoomHub.Types.DeepZoomImage.TileFormat (TileFormat)
import ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap)
import ZoomHub.Types.DeepZoomImage.TileSize (TileSize)
import ZoomHub.Types.VerificationToken (VerificationToken)

-- Connection
type Connection = K Connection.Connection Schemas

createConnectionPool ::
  (TimeUnit a, MonadUnliftIO io) =>
  ConnectInfo ->
  Integer ->
  a ->
  Integer ->
  io (P.Pool Connection)
createConnectionPool connInfo numStripes idleTime maxResourcesPerStripe =
  P.createConnectionPool
    (ConnectInfo.connectionString connInfo)
    (fromIntegral numStripes)
    (toNominalDiffTime idleTime)
    (fromIntegral maxResourcesPerStripe)

-- Reads: Content
-- getBy ::
--   params ~ '[ 'NotNull 'PGtext ] =>
--   (MonadUnliftIO m, MonadPQ Schemas m) =>
--   Condition 'Ungrouped '[] '[] Schemas _ _ ->
--   params ->
--   m (Maybe Content)
getBy :: (MonadUnliftIO m, MonadPQ Schemas m) => _ -> Text -> m (Maybe Content)
getBy condition parameter = do
  result <- executeParams (selectContentBy (\t -> t & where_ condition)) (Only parameter)
  firstRow result

getBy' ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  Condition 'Ungrouped '[] '[] Schemas '[ 'NotNull 'PGtext] _ ->
  Text ->
  m (Maybe Content)
getBy' _condition _parameter = pure Nothing

-- getBy' condition parameter = do
--   mContent <- getBy condition parameter
--   case mContent of
--     Just content -> do
--       -- Sample how often we count views to reduce database load:
--       -- http://stackoverflow.com/a/4762559/125305
--       let cId = contentId content
--       let numViews = contentNumViews content
--       let numViewsSampleRate = sampleRate numViews
--       numViewsSample <- liftIO $ randomRIO (1, numViewsSampleRate)
--       when (numViewsSample == 1) $
--         -- TODO: How can we run this async?
--         manipulateParams_ incrNumViews (numViewsSampleRate, cId)
--       return $ Just content
--     Nothing ->
--       return Nothing
--   where
--     sampleRate :: Int64 -> Int64
--     sampleRate numViews
--       | numViews < 100 = 1
--       | numViews < 1000 = 10
--       | numViews < 10000 = 100
--       | otherwise = 1000

incrNumViews :: Manipulation_ Schemas (Int64, Text) ()
incrNumViews = undefined

-- incrNumViews =
--   update_
--     #content
--     (Set (#num_views + param @1) `as` #num_views)
--     (#hash_id .== param @2)

{- ORMOLU_DISABLE -}
-- selectContentBy ::
--   ( TableExpression 'Ungrouped '[] '[] Schemas '[ 'NotNull a] _ ->
--     TableExpression 'Ungrouped '[] '[] Schemas '[ 'NotNull a] _
--   ) ->
--   Query '[] '[] Schemas '[ 'NotNull a] (RowPG ContentImageRow)


selectContentBy :: _ -> Statement Schemas (Only Text) Content
selectContentBy clauses = Query enc dec sql
  where
  enc = genericParams
  dec = decodeContent
  sql =
    select_
      ( #content ! #hash_id -- `as` #cirHashId
          :* #content ! #type_id -- `as` #cirTypeId
          :* #content ! #url -- `as` #cirURL
          :* #content ! #state -- `as` #cirState
          :* #content ! #initialized_at -- `as` #cirInitializedAt
          :* #content ! #active_at -- `as` #cirActiveAt
          :* #content ! #completed_at -- `as` #cirCompletedAt
          :* #content ! #title -- `as` #cirTitle
          :* #content ! #attribution_text -- `as` #cirAttributionText
          :* #content ! #attribution_link -- `as` #cirAttributionLink
          :* #content ! #mime -- `as` #cirMIME
          :* #content ! #size -- `as` #cirSize
          :* #content ! #error -- `as` #cirError
          :* #content ! #progress -- `as` #cirProgress
          :* #content ! #abuse_level_id -- `as` #cirAbuseLevelId
          :* #content ! #num_abuse_reports -- `as` #cirNumAbuseReports
          :* #content ! #num_views -- `as` #cirNumViews
          :* #content ! #version -- `as` #cirVersion
          :* #content ! #submitter_email -- `as` #cirSubmitterEmail
          :* #content ! #verification_token -- `as` #cirVerificationToken
          :* #content ! #verified_at -- `as` #cirVerifiedAt

          :* #image ! #width -- `as` #cirWidth
          :* #image ! #height -- `as` #cirHeight
          :* #image ! #tile_size -- `as` #cirTileSize
          :* #image ! #tile_overlap -- `as` #cirTileOverlap
          :* #image ! #tile_format -- `as` #cirTileFormat
      )
      ( from
          ( table #content
              & leftOuterJoin (table #image) (#content ! #id .== #image ! #content_id)
          )
        & clauses
      )


type ContentRow' =
  '[ "hash_id" ::: 'NotNull 'PGtext,
    "type_id" ::: 'NotNull 'PGint4,
    "url" ::: 'NotNull 'PGtext,
    "state" ::: 'NotNull 'PGtext,
    "initialized_at" ::: 'NotNull 'PGtimestamptz,
    "active_at" ::: 'Null 'PGtimestamptz,
    "completed_at" ::: 'Null 'PGtimestamptz,
    "title" ::: 'Null 'PGtext,
    "attribution_text" ::: 'Null 'PGtext,
    "attribution_link" ::: 'Null 'PGtext,
    "mime" ::: 'Null 'PGtext,
    "size" ::: 'Null 'PGint8,
    "error" ::: 'Null 'PGtext,
    "progress" ::: 'NotNull 'PGfloat8,
    "abuse_level_id" ::: 'NotNull 'PGint4,
    "num_abuse_reports" ::: 'NotNull 'PGint8,
    "num_views" ::: 'NotNull 'PGint8,
    "version" ::: 'NotNull 'PGint4,
    "submitter_email" ::: 'Null 'PGtext,
    "verification_token" ::: 'Null 'PGtext,
    "verified_at" ::: 'Null 'PGtimestamptz
  ]

type ImageRowNull' =
    '[ "width" ::: 'Null 'PGint8,
       "height" ::: 'Null 'PGint8,
       "tile_size" ::: 'Null 'PGint4,
       "tile_overlap" ::: 'Null 'PGint4,
       "tile_format" ::: 'Null 'PGtext
     ]

type ContentWithImageRow' = Join ContentRow' ImageRowNull'

decodeContent :: DecodeRow ContentWithImageRow' Content
decodeContent = do
  contentId <- #hash_id
  contentType <- #type_id
  contentURL <- #url
  contentState <- #state
  contentInitializedAt <- #initialized_at
  contentActiveAt <- #active_at
  contentCompletedAt <- #completed_at
  contentMIME <- #mime
  contentSize <- #size
  contentProgress <- #progress
  contentNumViews <- #num_views
  contentError <- #error
  contentSubmitterEmail <- #submitter_email
  contentVerificationToken <- #verification_token
  contentVerifiedAt <- #verified_at

  mWidth <- #width
  mHeight <- #height
  mTileSize <- #tile_size
  mTileOverlap <- #tile_overlap
  mTileFormat <- #tile_format
  let contentDZI = do
        width <- mWidth
        height <- mHeight
        tileSize <- mTileSize
        tileOverlap <- mTileOverlap
        tileFormat <- mTileFormat
        pure $ mkDeepZoomImage
          (fromIntegral (width :: Int64))
          (fromIntegral (height :: Int64))
          tileSize
          tileOverlap
          tileFormat
  return Content {..}
{- ORMOLU_ENABLE -}

-- Reads: Image
getImageById :: Int64 -> PQ Schemas Schemas IO (Maybe DeepZoomImage)
getImageById cId =
  executeParams
    (selectImageBy ((#image ! #content_id) .== param @1))
    (Only cId)
    >>= firstRow

selectImageBy ::
  Condition 'Ungrouped '[] '[] Schemas '[ 'NotNull 'PGint8] _ ->
  Statement Schemas (Only Int64) DeepZoomImage
selectImageBy condition = Query enc dec sql
  where
    enc = genericParams
    dec = decodeImage
    sql =
      {- ORMOLU_DISABLE -}
      select_
        (    #image ! #width
          :* #image ! #height
          :* #image ! #tile_size
          :* #image ! #tile_overlap
          :* #image ! #tile_format
        )
        (from (table #image) & where_ condition)
      {- ORMOLU_ENABLE -}

-- Writes: Image
createImage :: (MonadBaseControl IO m, MonadPQ db m) => Int64 -> UTCTime -> DeepZoomImage -> m Int64
createImage _cid _initializedAt _image = pure 0

-- createImage cid initializedAt image = do
--   let imageRow = imageToRow cid image initializedAt
--   result <- manipulateParams insertImage imageRow
--   fmap fromOnly . getRow 0 $ result

-- TODO: How can we avoid this duplication between `ContentRow`,
-- `ContentImageRow`, and `ImageRow`?
-- TODO: Look into Squeal’s `Join`.
data ContentImageRow = ContentImageRow
  { -- content
    cirHashId :: ContentId,
    cirTypeId :: ContentType,
    cirURL :: ContentURI,
    cirState :: ContentState,
    cirInitializedAt :: UTCTime,
    cirActiveAt :: Maybe UTCTime,
    cirCompletedAt :: Maybe UTCTime,
    cirTitle :: Maybe Text,
    cirAttributionText :: Maybe Text,
    cirAttributionLink :: Maybe Text,
    cirMIME :: Maybe ContentMIME,
    cirSize :: Maybe Int64,
    cirError :: Maybe Text,
    cirProgress :: Double,
    cirAbuseLevelId :: Int32,
    cirNumAbuseReports :: Int64,
    cirNumViews :: Int64,
    cirVersion :: Int32,
    cirSubmitterEmail :: Maybe Text, -- TODO: Introduce `Email` type
    cirVerificationToken :: Maybe VerificationToken,
    cirVerifiedAt :: Maybe UTCTime,
    -- image
    cirWidth :: Maybe Int64,
    cirHeight :: Maybe Int64,
    cirTileSize :: Maybe TileSize,
    cirTileOverlap :: Maybe TileOverlap,
    cirTileFormat :: Maybe TileFormat
  }
  deriving (Show, GHC.Generic)

instance SOP.Generic ContentImageRow

instance SOP.HasDatatypeInfo ContentImageRow

data ImageRow = ImageRow
  { irCreatedAt :: UTCTime,
    irWidth :: Int64,
    irHeight :: Int64,
    irTileSize :: TileSize,
    irTileOverlap :: TileOverlap,
    irTileFormat :: TileFormat
  }
  deriving (Show, GHC.Generic)

instance SOP.Generic ImageRow

instance SOP.HasDatatypeInfo ImageRow

-- See:
-- https://github.com/morphismtech/squeal/blob/0.9.1.3/RELEASE%20NOTES.md#:~:text=do%20custom%20encodings%20and%20decodings

type ImageRow' =
    '[ "width" ::: 'NotNull 'PGint8,
       "height" ::: 'NotNull 'PGint8,
       "tile_size" ::: 'NotNull 'PGint4,
       "tile_overlap" ::: 'NotNull 'PGint4,
       "tile_format" ::: 'NotNull 'PGtext
     ]

decodeImage :: DecodeRow ImageRow' DeepZoomImage
decodeImage = do
  width <- #width
  height <- #height
  tileSize <- #tile_size
  tileOverlap <- #tile_overlap
  tileFormat <- #tile_format
  return $
    mkDeepZoomImage
      (fromIntegral (width :: Int64))
      (fromIntegral (height :: Int64))
      tileSize
      tileOverlap
      tileFormat

-- -- TODO: How to combine this with the above?
-- type ImageRowNull' =
--     '[ "width" ::: 'Null 'PGint8,
--        "height" ::: 'Null 'PGint8,
--        "tile_size" ::: 'Null 'PGint4,
--        "tile_overlap" ::: 'Null 'PGint4,
--        "tile_format" ::: 'Null 'PGtext
--      ]

-- decodeImage' :: DecodeRow ImageRowNull' (Maybe DeepZoomImage)
-- decodeImage' = runMaybeT $ do
--   width <- #width
--   height <- #height
--   tileSize <- #tile_size
--   tileOverlap <- #tile_overlap
--   tileFormat <- #tile_format
--   return $
--     mkDeepZoomImage
--       (fromIntegral (width :: Int64))
--       (fromIntegral (height :: Int64))
--       tileSize
--       tileOverlap
--       tileFormat

contentImageRowToContent :: ContentImageRow -> Content
contentImageRowToContent cr =
  Content
    { contentId = cirHashId cr,
      contentType = cirTypeId cr,
      contentURL = cirURL cr,
      contentState = cirState cr,
      contentInitializedAt = cirInitializedAt cr,
      contentActiveAt = cirActiveAt cr,
      contentCompletedAt = cirCompletedAt cr,
      contentMIME = cirMIME cr,
      contentSize = fromIntegral <$> cirSize cr,
      contentProgress = cirProgress cr,
      contentNumViews = fromIntegral (cirNumViews cr),
      contentError = cirError cr,
      contentDZI = mDZI,
      contentSubmitterEmail = cirSubmitterEmail cr,
      contentVerificationToken = cirVerificationToken cr,
      contentVerifiedAt = cirVerifiedAt cr
    }
  where
    mDZI =
      mkDeepZoomImage
        <$> (fromIntegral <$> cirWidth cr)
        <*> (fromIntegral <$> cirHeight cr)
        <*> cirTileSize cr
        <*> cirTileOverlap cr
        <*> cirTileFormat cr


contentRowToContent :: ContentRow -> Content
contentRowToContent result =
  Content
    { contentId = crHashId result,
      contentType = crTypeId result,
      contentURL = crURL result,
      contentState = crState result,
      contentInitializedAt = crInitializedAt result,
      contentActiveAt = crActiveAt result,
      contentCompletedAt = crCompletedAt result,
      contentMIME = crMIME result,
      contentSize = crSize result,
      contentProgress = crProgress result,
      contentNumViews = crNumViews result,
      contentError = crError result,
      contentDZI = Nothing,
      contentSubmitterEmail = crSubmitterEmail result,
      contentVerificationToken = crVerificationToken result,
      contentVerifiedAt = crVerifiedAt result
    }

data ContentRow = ContentRow
  { crHashId :: ContentId, --  1
    crTypeId :: ContentType, --  2
    crURL :: ContentURI, --  3
    crState :: ContentState, --  4
    crInitializedAt :: UTCTime, --  5
    crActiveAt :: Maybe UTCTime, --  6
    crCompletedAt :: Maybe UTCTime, --  7
    crTitle :: Maybe Text, --  8
    crAttributionText :: Maybe Text, --  9
    crAttributionLink :: Maybe Text, -- 10
    crMIME :: Maybe ContentMIME, -- 11
    crSize :: Maybe Int64, -- 12
    crError :: Maybe Text, -- 13
    crProgress :: Double, -- 14
    crAbuseLevelId :: Int32, -- 15
    crNumAbuseReports :: Int64, -- 16
    crNumViews :: Int64, -- 17
    crVersion :: Int32, -- 18
    crSubmitterEmail :: Maybe Text, -- 19
    crVerificationToken :: Maybe VerificationToken, -- 20
    crVerifiedAt :: Maybe UTCTime -- 21
  }
  deriving (Show, GHC.Generic)

instance SOP.Generic ContentRow

instance SOP.HasDatatypeInfo ContentRow

{- ORMOLU_DISABLE -}
insertContent :: Manipulation_ Schemas (Text, Maybe Text, Maybe Text) ContentRow
insertContent =
  insertInto
    #content
    ( Values_
        ( Default `as` #id
            :* Set "$placeholder-overwritten-by-trigger$" `as` #hash_id
            :* Default `as` #type_id
            :* Set (param @1) `as` #url
            :* Default `as` #state
            :* Default `as` #initialized_at
            :* Default `as` #active_at
            :* Default `as` #completed_at
            :* Default `as` #title
            :* Default `as` #attribution_text
            :* Default `as` #attribution_link
            :* Default `as` #mime
            :* Default `as` #size
            :* Default `as` #error
            :* Default `as` #progress
            :* Default `as` #abuse_level_id
            :* Default `as` #num_abuse_reports
            :* Default `as` #num_views
            :* Set (inline Content.version) `as` #version
            :* Set (param @2) `as` #submitter_email
            :* Set (param @3) `as` #verification_token
            :* Default `as` #verified_at
        )
    )
    OnConflictDoRaise
    ( Returning_
        ( #hash_id `as` #crHashId
            :* #type_id `as` #crTypeId
            :* #url `as` #crURL
            :* #state `as` #crState
            :* #initialized_at `as` #crInitializedAt
            :* #active_at `as` #crActiveAt
            :* #completed_at `as` #crCompletedAt
            :* #title `as` #crTitle
            :* #attribution_text `as` #crAttributionText
            :* #attribution_link `as` #crAttributionLink
            :* #mime `as` #crMIME
            :* #size `as` #crSize
            :* #error `as` #crError
            :* #progress `as` #crProgress
            :* #abuse_level_id `as` #crAbuseLevelId
            :* #num_abuse_reports `as` #crNumAbuseReports
            :* #num_views `as` #crNumViews
            :* #version `as` #crVersion
            :* #submitter_email `as` #crSubmitterEmail
            :* #verification_token `as` #crVerificationToken
            :* #verified_at `as` #crVerifiedAt
        )
    )
{- ORMOLU_ENABLE -}

markContentAsActive :: Manipulation_ Schemas (Only Text) ContentRow
markContentAsActive = undefined

-- markContentAsActive =
--   update
--     #content
--     ( Set (inline Unknown) `as` #type_id
--         :* Set (inline Active) `as` #state
--         :* Set currentTimestamp `as` #active_at
--         :* Set null_ `as` #completed_at
--         :* Set null_ `as` #title
--         :* Set null_ `as` #attribution_text
--         :* Set null_ `as` #attribution_link
--         :* Set null_ `as` #mime
--         :* Set null_ `as` #size
--         :* Set null_ `as` #error
--         :* Set 0.0 `as` #progress
--     )
--     (#hash_id .== param @1)
--     ( Returning_
--         ( #hash_id `as` #crHashId
--             :* #type_id `as` #crTypeId
--             :* #url `as` #crURL
--             :* #state `as` #crState
--             :* #initialized_at `as` #crInitializedAt
--             :* #active_at `as` #crActiveAt
--             :* #completed_at `as` #crCompletedAt
--             :* #title `as` #crTitle
--             :* #attribution_text `as` #crAttributionText
--             :* #attribution_link `as` #crAttributionLink
--             :* #mime `as` #crMIME
--             :* #size `as` #crSize
--             :* #error `as` #crError
--             :* #progress `as` #crProgress
--             :* #abuse_level_id `as` #crAbuseLevelId
--             :* #num_abuse_reports `as` #crNumAbuseReports
--             :* #num_views `as` #crNumViews
--             :* #version `as` #crVersion
--             :* #submitter_email `as` #crSubmitterEmail
--             :* #verification_token `as` #crVerificationToken
--             :* #verified_at `as` #crVerifiedAt
--         )
--     )

markContentAsFailure :: Manipulation_ Schemas (Text, Maybe Text) ContentRow
markContentAsFailure = undefined

-- markContentAsFailure =
--   update
--     #content
--     ( Set (inline Unknown) `as` #type_id
--         :* Set (inline CompletedFailure) `as` #state
--         :* Set currentTimestamp `as` #completed_at
--         :* Set null_ `as` #title
--         :* Set null_ `as` #attribution_text
--         :* Set null_ `as` #attribution_link
--         :* Set null_ `as` #mime
--         :* Set null_ `as` #size
--         :* Set (param @2) `as` #error
--         :* Set 1.0 `as` #progress
--     )
--     (#hash_id .== param @1)
--     ( Returning_
--         ( #hash_id `as` #crHashId
--             :* #type_id `as` #crTypeId
--             :* #url `as` #crURL
--             :* #state `as` #crState
--             :* #initialized_at `as` #crInitializedAt
--             :* #active_at `as` #crActiveAt
--             :* #completed_at `as` #crCompletedAt
--             :* #title `as` #crTitle
--             :* #attribution_text `as` #crAttributionText
--             :* #attribution_link `as` #crAttributionLink
--             :* #mime `as` #crMIME
--             :* #size `as` #crSize
--             :* #error `as` #crError
--             :* #progress `as` #crProgress
--             :* #abuse_level_id `as` #crAbuseLevelId
--             :* #num_abuse_reports `as` #crNumAbuseReports
--             :* #num_views `as` #crNumViews
--             :* #version `as` #crVersion
--             :* #submitter_email `as` #crSubmitterEmail
--             :* #verification_token `as` #crVerificationToken
--             :* #verified_at `as` #crVerifiedAt
--         )
--     )

markContentAsSuccess ::
  Manipulation_
    Schemas
    (Text, Maybe Text, Maybe Int64)
    ContentRow
markContentAsSuccess = undefined

-- markContentAsSuccess =
--   update
--     #content
--     ( Set (inline Image) `as` #type_id
--         :* Set (inline CompletedSuccess) `as` #state
--         :* Set currentTimestamp `as` #completed_at
--         :* Set (param @2) `as` #mime
--         :* Set (param @3) `as` #size
--         :* Set null_ `as` #error
--         :* Set 1.0 `as` #progress -- reset any previous errors
--     )
--     (#hash_id .== param @1)
--     ( Returning_
--         ( #hash_id `as` #crHashId
--             :* #type_id `as` #crTypeId
--             :* #url `as` #crURL
--             :* #state `as` #crState
--             :* #initialized_at `as` #crInitializedAt
--             :* #active_at `as` #crActiveAt
--             :* #completed_at `as` #crCompletedAt
--             :* #title `as` #crTitle
--             :* #attribution_text `as` #crAttributionText
--             :* #attribution_link `as` #crAttributionLink
--             :* #mime `as` #crMIME
--             :* #size `as` #crSize
--             :* #error `as` #crError
--             :* #progress `as` #crProgress
--             :* #abuse_level_id `as` #crAbuseLevelId
--             :* #num_abuse_reports `as` #crNumAbuseReports
--             :* #num_views `as` #crNumViews
--             :* #version `as` #crVersion
--             :* #submitter_email `as` #crSubmitterEmail
--             :* #verification_token `as` #crVerificationToken
--             :* #verified_at `as` #crVerifiedAt
--         )
--     )

markContentAsVerified :: Manipulation_ Schemas (Only Text) ContentRow
markContentAsVerified = undefined

-- markContentAsVerified =
--   update
--     #content
--     ( Set currentTimestamp `as` #verified_at
--         :* Set (inline Content.version) `as` #version
--     )
--     (#hash_id .== param @1)
--     ( Returning_
--         ( #hash_id `as` #crHashId
--             :* #type_id `as` #crTypeId
--             :* #url `as` #crURL
--             :* #state `as` #crState
--             :* #initialized_at `as` #crInitializedAt
--             :* #active_at `as` #crActiveAt
--             :* #completed_at `as` #crCompletedAt
--             :* #title `as` #crTitle
--             :* #attribution_text `as` #crAttributionText
--             :* #attribution_link `as` #crAttributionLink
--             :* #mime `as` #crMIME
--             :* #size `as` #crSize
--             :* #error `as` #crError
--             :* #progress `as` #crProgress
--             :* #abuse_level_id `as` #crAbuseLevelId
--             :* #num_abuse_reports `as` #crNumAbuseReports
--             :* #num_views `as` #crNumViews
--             :* #version `as` #crVersion
--             :* #submitter_email `as` #crSubmitterEmail
--             :* #verification_token `as` #crVerificationToken
--             :* #verified_at `as` #crVerifiedAt
--         )
--     )

resetContentAsInitialized :: Manipulation_ Schemas (Only Text) ContentRow
resetContentAsInitialized = undefined

-- resetContentAsInitialized =
--   update
--     #content
--     ( Set (inline Unknown) `as` #type_id
--         :* Set (inline Initialized) `as` #state
--         :* Set null_ `as` #active_at
--         :* Set null_ `as` #completed_at
--         :* Set null_ `as` #mime
--         :* Set null_ `as` #size
--         :* Set null_ `as` #error -- reset any previous errors
--         :* Set 0.0 `as` #progress
--     )
--     (#hash_id .== param @1)
--     ( Returning_
--         ( #hash_id `as` #crHashId
--             :* #type_id `as` #crTypeId
--             :* #url `as` #crURL
--             :* #state `as` #crState
--             :* #initialized_at `as` #crInitializedAt
--             :* #active_at `as` #crActiveAt
--             :* #completed_at `as` #crCompletedAt
--             :* #title `as` #crTitle
--             :* #attribution_text `as` #crAttributionText
--             :* #attribution_link `as` #crAttributionLink
--             :* #mime `as` #crMIME
--             :* #size `as` #crSize
--             :* #error `as` #crError
--             :* #progress `as` #crProgress
--             :* #abuse_level_id `as` #crAbuseLevelId
--             :* #num_abuse_reports `as` #crNumAbuseReports
--             :* #num_views `as` #crNumViews
--             :* #version `as` #crVersion
--             :* #submitter_email `as` #crSubmitterEmail
--             :* #verification_token `as` #crVerificationToken
--             :* #verified_at `as` #crVerifiedAt
--         )
--     )

imageToInsertRow :: ContentId -> DeepZoomImage -> InsertImageRow
imageToInsertRow cid dzi =
  InsertImageRow
    { iirContentId = cid,
      iirWidth = fromIntegral . dziWidth $ dzi,
      iirHeight = fromIntegral . dziHeight $ dzi,
      iirTileSize = dziTileSize dzi,
      iirTileOverlap = dziTileOverlap dzi,
      iirTileFormat = dziTileFormat dzi
    }

data InsertImageRow = InsertImageRow
  { iirContentId :: ContentId,
    iirWidth :: Int64,
    iirHeight :: Int64,
    iirTileSize :: TileSize,
    iirTileOverlap :: TileOverlap,
    iirTileFormat :: TileFormat
  }
  deriving (Show, GHC.Generic)

instance SOP.Generic InsertImageRow

instance SOP.HasDatatypeInfo InsertImageRow

insertImage :: Manipulation_ Schemas InsertImageRow ()
insertImage = undefined

-- insertImage =
--   insertInto_
--     #image
--     ( Subquery
--         ( select_
--             ( #content ! #id `as` #content_id
--                 :* currentTimestamp `as` #created_at
--                 :* param @2 `as` #width
--                 :* param @3 `as` #height
--                 :* param @4 `as` #tile_size
--                 :* param @5 `as` #tile_overlap
--                 :* param @6 `as` #tile_format
--             )
--             ( from (table #content)
--                 & where_ (#content ! #hash_id .== param @1)
--             )
--         )
--     )

deleteImage :: Manipulation_ Schemas (Only Text) ()
deleteImage = undefined

-- deleteImage =
--   deleteFrom
--     #image
--     (Using (table #content))
--     ((#content ! #hash_id .== param @1) .&& (#image ! #content_id .== #content ! #id))
--     (Returning_ Nil)

-- Unsafe
unsafeCreateContent ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  Content ->
  m (Maybe Content)
unsafeCreateContent _content = undefined

-- unsafeCreateContent content =
--   transactionally_ $ do
--     result <- manipulateParams unsafeInsertContent (contentToRow content)
--     contentRow <- firstRow result
--     pure $ contentRowToContent <$> contentRow

unsafeInsertContent :: Manipulation_ Schemas ContentRow ContentRow
unsafeInsertContent = undefined

-- unsafeInsertContent =
--   insertInto
--     #content
--     ( Values_
--         ( Default `as` #id
--             :* Set (param @1) `as` #hash_id
--             :* Set (param @2) `as` #type_id
--             :* Set (param @3) `as` #url
--             :* Set (param @4) `as` #state
--             :* Set (param @5) `as` #initialized_at
--             :* Set (param @6) `as` #active_at
--             :* Set (param @7) `as` #completed_at
--             :* Set (param @8) `as` #title
--             :* Set (param @9) `as` #attribution_text
--             :* Set (param @10) `as` #attribution_link
--             :* Set (param @11) `as` #mime
--             :* Set (param @12) `as` #size
--             :* Set (param @13) `as` #error
--             :* Set (param @14) `as` #progress
--             :* Set (param @15) `as` #abuse_level_id
--             :* Set (param @16) `as` #num_abuse_reports
--             :* Set (param @17) `as` #num_views
--             :* Set (param @18) `as` #version
--             :* Set (param @19) `as` #submitter_email
--             :* Set (param @20) `as` #verification_token
--             :* Set (param @21) `as` #verified_at
--         )
--     )
--     OnConflictDoRaise
--     ( Returning_
--         ( #hash_id `as` #crHashId
--             :* #type_id `as` #crTypeId
--             :* #url `as` #crURL
--             :* #state `as` #crState
--             :* #initialized_at `as` #crInitializedAt
--             :* #active_at `as` #crActiveAt
--             :* #completed_at `as` #crCompletedAt
--             :* #title `as` #crTitle
--             :* #attribution_text `as` #crAttributionText
--             :* #attribution_link `as` #crAttributionLink
--             :* #mime `as` #crMIME
--             :* #size `as` #crSize
--             :* #error `as` #crError
--             :* #progress `as` #crProgress
--             :* #abuse_level_id `as` #crAbuseLevelId
--             :* #num_abuse_reports `as` #crNumAbuseReports
--             :* #num_views `as` #crNumViews
--             :* #version `as` #crVersion
--             :* #submitter_email `as` #crSubmitterEmail
--             :* #verification_token `as` #crVerificationToken
--             :* #verified_at `as` #crVerifiedAt
--         )
--     )

-- contentToRow :: Content -> ContentRow
-- contentToRow c =
--   ContentRow
--     { crHashId = contentId c,
--       crTypeId = contentType c,
--       crURL = contentURL c,
--       crState = contentState c,
--       crInitializedAt = contentInitializedAt c,
--       crActiveAt = contentActiveAt c,
--       crCompletedAt = contentCompletedAt c,
--       crTitle = Nothing,
--       crAttributionText = Nothing,
--       crAttributionLink = Nothing,
--       crMIME = contentMIME c,
--       crSize = contentSize c,
--       crError = contentError c,
--       crProgress = contentProgress c,
--       crAbuseLevelId = 0, -- TODO: Replace hard-coded value
--       crNumAbuseReports = 0,
--       crNumViews = contentNumViews c,
--       crVersion = Content.version,
--       crSubmitterEmail = contentSubmitterEmail c,
--       crVerificationToken = contentVerificationToken c,
--       crVerifiedAt = contentVerifiedAt c
--     }

toNominalDiffTime :: (TimeUnit a) => a -> NominalDiffTime
toNominalDiffTime duration =
  fromIntegral $
    toMicroseconds duration `div` toMicroseconds (1 :: Second)
