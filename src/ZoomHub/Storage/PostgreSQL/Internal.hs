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
import Control.Monad.Catch (MonadMask)
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
    EncodeParams,
    GenericParams (genericParams),
    GenericRow (genericRow),
    Grouping (Ungrouped),
    Join,
    Manipulation_,
    MonadPQ (executeParams),
    NP (Nil, (:*)),
    NullType (NotNull, Null),
    Only (..),
    Optional (Default, Set),
    PGType (PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz),
    PQ,
    QueryClause (Subquery),
    Statement (Manipulation, Query),
    TableExpression,
    UsingClause (NoUsing, Using),
    as,
    currentTimestamp,
    deleteFrom,
    firstRow,
    from,
    inline,
    insertInto,
    insertInto_,
    leftOuterJoin,
    manipulateParams_,
    null_,
    param,
    select_,
    table,
    transactionally_,
    update,
    update_,
    where_,
    (!),
    (&),
    (*.),
    (.&&),
    (.*),
    (.==),
    (:::),
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
getBy ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  Condition 'Ungrouped '[] '[] Schemas _ _ ->
  Text -> -- TODO: Make generic
  m (Maybe Content)
getBy condition parameter = do
  result <- executeParams (selectContentBy (\t -> t & where_ condition)) (Only parameter)
  firstRow result

getBy' ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  Condition 'Ungrouped '[] '[] Schemas '[ 'NotNull 'PGtext] _ ->
  Text ->
  m (Maybe Content)
getBy' condition parameter = do
  mContent <- getBy condition parameter
  case mContent of
    Just content -> do
      -- Sample how often we count views to reduce database load:
      -- http://stackoverflow.com/a/4762559/125305
      let cId = contentId content
          numViews = contentNumViews content
          numViewsSampleRate = sampleRate numViews
      numViewsSample <- liftIO $ randomRIO (1, numViewsSampleRate)
      when (numViewsSample == 1) $
        -- TODO: How can we run this async?
        manipulateParams_ incrNumViews (numViewsSampleRate, cId)
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

incrNumViews :: Manipulation_ Schemas (Int64, Text) ()
incrNumViews =
  update_
    #content
    (Set (#num_views + param @1) `as` #num_views)
    (#hash_id .== param @2)

selectContentBy ::
  ( TableExpression 'Ungrouped '[] '[] Schemas '[ 'NotNull 'PGtext] _ ->
    TableExpression 'Ungrouped '[] '[] Schemas '[ 'NotNull 'PGtext] _
  ) ->
  Statement Schemas (Only Text) Content
selectContentBy clauses = Query enc dec sql
  where
    enc = genericParams
    dec = decodeContentWithImage
    sql =
      select_
        ( (#content ! #hash_id)
            :* (#content ! #type_id)
            :* (#content ! #url)
            :* (#content ! #state)
            :* (#content ! #initialized_at)
            :* (#content ! #active_at)
            :* (#content ! #completed_at)
            :* (#content ! #title)
            :* (#content ! #attribution_text)
            :* (#content ! #attribution_link)
            :* (#content ! #mime)
            :* (#content ! #size)
            :* (#content ! #error)
            :* (#content ! #progress)
            :* (#content ! #abuse_level_id)
            :* (#content ! #num_abuse_reports)
            :* (#content ! #num_views)
            :* (#content ! #version)
            :* (#content ! #submitter_email)
            :* (#content ! #verification_token)
            :* (#content ! #verified_at)
            :* (#image ! #width)
            :* (#image ! #height)
            :* (#image ! #tile_size)
            :* (#image ! #tile_overlap)
            :* (#image ! #tile_format)
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

encodeContentRow :: EncodeParams Schemas _ ContentRow
encodeContentRow =
  crHashId
    .* crTypeId
    .* crURL
    .* crState
    .* crInitializedAt
    .* crActiveAt
    .* crCompletedAt
    .* crTitle
    .* crAttributionText
    .* crAttributionLink
    .* crMIME
    .* crSize
    .* crError
    .* crProgress
    .* crAbuseLevelId
    .* crNumAbuseReports
    .* crNumViews
    .* crVersion
    .* crSubmitterEmail
    .* crVerificationToken
    *. crVerifiedAt

decodeContent :: DecodeRow ContentRow' Content
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

  let contentDZI = Nothing
  return Content {..}

decodeContentWithImage :: DecodeRow ContentWithImageRow' Content
decodeContentWithImage = do
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
        pure $
          mkDeepZoomImage
            (fromIntegral (width :: Int64))
            (fromIntegral (height :: Int64))
            tileSize
            tileOverlap
            tileFormat
  return Content {..}

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
      select_
        ( (#image ! #width)
            :* (#image ! #height)
            :* (#image ! #tile_size)
            :* (#image ! #tile_overlap)
            :* (#image ! #tile_format)
        )
        (from (table #image) & where_ condition)

-- -- Writes: Image
-- createImage ::
--   (MonadBaseControl IO m, MonadPQ db m) =>
--   Int64 ->
--   UTCTime ->
--   DeepZoomImage ->
--   m Int64
-- createImage cid initializedAt image = do
--   let imageRow = imageToRow cid image initializedAt
--   result <- manipulateParams insertImage imageRow
--   fmap fromOnly . getRow 0 $ result

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

insertContent :: Statement Schemas (ContentURI, Maybe Text, Maybe Text) Content
insertContent = Manipulation encode decode sql
  where
    encode = genericParams
    decode = decodeContent
    sql =
      insertInto
        #content
        ( Values_
            ( (Default `as` #id)
                :* (Set "$placeholder-overwritten-by-trigger$" `as` #hash_id)
                :* (Default `as` #type_id)
                :* (Set (param @1) `as` #url)
                :* (Default `as` #state)
                :* (Default `as` #initialized_at)
                :* (Default `as` #active_at)
                :* (Default `as` #completed_at)
                :* (Default `as` #title)
                :* (Default `as` #attribution_text)
                :* (Default `as` #attribution_link)
                :* (Default `as` #mime)
                :* (Default `as` #size)
                :* (Default `as` #error)
                :* (Default `as` #progress)
                :* (Default `as` #abuse_level_id)
                :* (Default `as` #num_abuse_reports)
                :* (Default `as` #num_views)
                :* (Set (inline Content.version) `as` #version)
                :* (Set (param @2) `as` #submitter_email)
                :* (Set (param @3) `as` #verification_token)
                :* (Default `as` #verified_at)
            )
        )
        OnConflictDoRaise
        ( Returning_
            ( #hash_id
                :* #type_id
                :* #url
                :* #state
                :* #initialized_at
                :* #active_at
                :* #completed_at
                :* #title
                :* #attribution_text
                :* #attribution_link
                :* #mime
                :* #size
                :* #error
                :* #progress
                :* #abuse_level_id
                :* #num_abuse_reports
                :* #num_views
                :* #version
                :* #submitter_email
                :* #verification_token
                :* #verified_at
            )
        )

markContentAsActive :: Statement Schemas (Only ContentId) Content
markContentAsActive = Manipulation encode decode sql
  where
    encode = genericParams
    decode = decodeContent
    sql =
      update
        #content
        ( (Set (inline Unknown) `as` #type_id)
            :* (Set (inline Active) `as` #state)
            :* (Set currentTimestamp `as` #active_at)
            :* (Set null_ `as` #completed_at)
            :* (Set null_ `as` #title)
            :* (Set null_ `as` #attribution_text)
            :* (Set null_ `as` #attribution_link)
            :* (Set null_ `as` #mime)
            :* (Set null_ `as` #size)
            :* (Set null_ `as` #error)
            :* (Set 0.0 `as` #progress)
        )
        NoUsing
        (#hash_id .== param @1)
        ( Returning_
            ( #hash_id
                :* #type_id
                :* #url
                :* #state
                :* #initialized_at
                :* #active_at
                :* #completed_at
                :* #title
                :* #attribution_text
                :* #attribution_link
                :* #mime
                :* #size
                :* #error
                :* #progress
                :* #abuse_level_id
                :* #num_abuse_reports
                :* #num_views
                :* #version
                :* #submitter_email
                :* #verification_token
                :* #verified_at
            )
        )

markContentAsFailure :: Statement Schemas (ContentId, Maybe Text) Content
markContentAsFailure = Manipulation encode decode sql
  where
    encode = genericParams
    decode = decodeContent
    sql =
      update
        #content
        ( (Set (inline Unknown) `as` #type_id)
            :* (Set (inline CompletedFailure) `as` #state)
            :* (Set currentTimestamp `as` #completed_at)
            :* (Set null_ `as` #title)
            :* (Set null_ `as` #attribution_text)
            :* (Set null_ `as` #attribution_link)
            :* (Set null_ `as` #mime)
            :* (Set null_ `as` #size)
            :* (Set (param @2) `as` #error)
            :* (Set 1.0 `as` #progress)
        )
        NoUsing
        (#hash_id .== param @1)
        ( Returning_
            ( #hash_id
                :* #type_id
                :* #url
                :* #state
                :* #initialized_at
                :* #active_at
                :* #completed_at
                :* #title
                :* #attribution_text
                :* #attribution_link
                :* #mime
                :* #size
                :* #error
                :* #progress
                :* #abuse_level_id
                :* #num_abuse_reports
                :* #num_views
                :* #version
                :* #submitter_email
                :* #verification_token
                :* #verified_at
            )
        )

markContentAsSuccess ::
  Statement Schemas (ContentId, Maybe ContentMIME, Maybe Int64) Content
markContentAsSuccess = Manipulation encode decode sql
  where
    encode = genericParams
    decode = decodeContent
    sql =
      update
        #content
        ( (Set (inline Image) `as` #type_id)
            :* (Set (inline CompletedSuccess) `as` #state)
            :* (Set currentTimestamp `as` #completed_at)
            :* (Set (param @2) `as` #mime)
            :* (Set (param @3) `as` #size)
            :* (Set null_ `as` #error) -- reset any previous errors
            :* (Set 1.0 `as` #progress)
        )
        NoUsing
        (#hash_id .== param @1)
        ( Returning_
            ( #hash_id
                :* #type_id
                :* #url
                :* #state
                :* #initialized_at
                :* #active_at
                :* #completed_at
                :* #title
                :* #attribution_text
                :* #attribution_link
                :* #mime
                :* #size
                :* #error
                :* #progress
                :* #abuse_level_id
                :* #num_abuse_reports
                :* #num_views
                :* #version
                :* #submitter_email
                :* #verification_token
                :* #verified_at
            )
        )

markContentAsVerified :: Statement Schemas (Only ContentId) Content
markContentAsVerified = Manipulation encode decode sql
  where
    encode = genericParams
    decode = decodeContent
    sql =
      update
        #content
        ( (Set currentTimestamp `as` #verified_at)
            :* (Set (inline Content.version) `as` #version)
        )
        NoUsing
        (#hash_id .== param @1)
        ( Returning_
            ( #hash_id
                :* #type_id
                :* #url
                :* #state
                :* #initialized_at
                :* #active_at
                :* #completed_at
                :* #title
                :* #attribution_text
                :* #attribution_link
                :* #mime
                :* #size
                :* #error
                :* #progress
                :* #abuse_level_id
                :* #num_abuse_reports
                :* #num_views
                :* #version
                :* #submitter_email
                :* #verification_token
                :* #verified_at
            )
        )

resetContentAsInitialized :: Statement Schemas (Only ContentId) Content
resetContentAsInitialized = Manipulation encode decode sql
  where
    encode = genericParams
    decode = decodeContent
    sql =
      update
        #content
        ( (Set (inline Unknown) `as` #type_id)
            :* (Set (inline Initialized) `as` #state)
            :* (Set null_ `as` #active_at)
            :* (Set null_ `as` #completed_at)
            :* (Set null_ `as` #mime)
            :* (Set null_ `as` #size)
            :* (Set null_ `as` #error) -- reset any previous errors
            :* (Set 0.0 `as` #progress)
        )
        NoUsing
        (#hash_id .== param @1)
        ( Returning_
            ( #hash_id
                :* #type_id
                :* #url
                :* #state
                :* #initialized_at
                :* #active_at
                :* #completed_at
                :* #title
                :* #attribution_text
                :* #attribution_link
                :* #mime
                :* #size
                :* #error
                :* #progress
                :* #abuse_level_id
                :* #num_abuse_reports
                :* #num_views
                :* #version
                :* #submitter_email
                :* #verification_token
                :* #verified_at
            )
        )

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

insertImage :: Statement Schemas InsertImageRow ()
insertImage = Manipulation encode decode sql
  where
    encode = genericParams
    decode = genericRow
    sql =
      insertInto_
        #image
        ( Subquery
            ( select_
                ( (#content ! #id `as` #content_id)
                    :* (currentTimestamp `as` #created_at)
                    :* (param @2 `as` #width)
                    :* (param @3 `as` #height)
                    :* (param @4 `as` #tile_size)
                    :* (param @5 `as` #tile_overlap)
                    :* (param @6 `as` #tile_format)
                )
                ( from (table #content)
                    & where_ (#content ! #hash_id .== param @1)
                )
            )
        )

deleteImage :: Statement Schemas (Only ContentId) ()
deleteImage = Manipulation encode decode sql
  where
    encode = genericParams
    decode = genericRow
    sql =
      deleteFrom
        #image
        (Using (table #content))
        ( (#content ! #hash_id .== param @1)
            .&& (#image ! #content_id .== #content ! #id)
        )
        (Returning_ Nil)

-- Unsafe
unsafeCreateContent ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  Content ->
  m (Maybe Content)
unsafeCreateContent content =
  transactionally_ $ do
    result <- executeParams unsafeInsertContent (contentToRow content)
    firstRow result

unsafeInsertContent :: Statement Schemas ContentRow Content
unsafeInsertContent = Manipulation encode decode sql
  where
    encode = encodeContentRow
    decode = decodeContent
    sql =
      insertInto
        #content
        ( Values_
            ( (Default `as` #id)
                :* (Set (param @1) `as` #hash_id)
                :* (Set (param @2) `as` #type_id)
                :* (Set (param @3) `as` #url)
                :* (Set (param @4) `as` #state)
                :* (Set (param @5) `as` #initialized_at)
                :* (Set (param @6) `as` #active_at)
                :* (Set (param @7) `as` #completed_at)
                :* (Set (param @8) `as` #title)
                :* (Set (param @9) `as` #attribution_text)
                :* (Set (param @10) `as` #attribution_link)
                :* (Set (param @11) `as` #mime)
                :* (Set (param @12) `as` #size)
                :* (Set (param @13) `as` #error)
                :* (Set (param @14) `as` #progress)
                :* (Set (param @15) `as` #abuse_level_id)
                :* (Set (param @16) `as` #num_abuse_reports)
                :* (Set (param @17) `as` #num_views)
                :* (Set (param @18) `as` #version)
                :* (Set (param @19) `as` #submitter_email)
                :* (Set (param @20) `as` #verification_token)
                :* (Set (param @21) `as` #verified_at)
            )
        )
        OnConflictDoRaise
        ( Returning_
            ( #hash_id
                :* #type_id
                :* #url
                :* #state
                :* #initialized_at
                :* #active_at
                :* #completed_at
                :* #title
                :* #attribution_text
                :* #attribution_link
                :* #mime
                :* #size
                :* #error
                :* #progress
                :* #abuse_level_id
                :* #num_abuse_reports
                :* #num_views
                :* #version
                :* #submitter_email
                :* #verification_token
                :* #verified_at
            )
        )

contentToRow :: Content -> ContentRow
contentToRow c =
  ContentRow
    { crHashId = contentId c,
      crTypeId = contentType c,
      crURL = contentURL c,
      crState = contentState c,
      crInitializedAt = contentInitializedAt c,
      crActiveAt = contentActiveAt c,
      crCompletedAt = contentCompletedAt c,
      crTitle = Nothing,
      crAttributionText = Nothing,
      crAttributionLink = Nothing,
      crMIME = contentMIME c,
      crSize = contentSize c,
      crError = contentError c,
      crProgress = contentProgress c,
      crAbuseLevelId = 0, -- TODO: Replace hard-coded value
      crNumAbuseReports = 0,
      crNumViews = contentNumViews c,
      crVersion = Content.version,
      crSubmitterEmail = contentSubmitterEmail c,
      crVerificationToken = contentVerificationToken c,
      crVerifiedAt = contentVerifiedAt c
    }

toNominalDiffTime :: (TimeUnit a) => a -> NominalDiffTime
toNominalDiffTime duration =
  fromIntegral $
    toMicroseconds duration `div` toMicroseconds (1 :: Second)
