{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas -fno-specialise -fno-full-laziness #-}

module ZoomHub.Storage.PostgreSQL
  ( -- ** Schema
    Schemas,

    -- ** Connection
    Connection,
    createConnectionPool,

    -- ** Read operations
    getById,
    getByURL,
    getNextUnprocessed,
    getExpiredActive,

    -- ** Read operations (with view tracking)
    getById',
    getByURL',

    -- ** Write operations
    initialize,
    markAsActive,
    markAsFailure,
    markAsSuccess,
    resetAsInitialized,
    dequeueNextUnprocessed,
    module ZoomHub.Storage.PostgreSQL.ConnectInfo,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Units (TimeUnit)
import qualified Data.UUID.V4 as UUIDV4
import Squeal.PostgreSQL
  ( (!),
    (&),
    (.&&),
    (.<),
    (.==),
    MonadPQ,
    Only (Only),
    SortExpression (Asc, Desc),
    firstRow,
    getRows,
    limit,
    literal,
    manipulateParams,
    manipulateParams_,
    orderBy,
    param,
    runQueryParams,
    transactionally_,
    where_,
  )
import UnliftIO (MonadUnliftIO, liftIO)
import ZoomHub.Storage.PostgreSQL.ConnectInfo
  ( ConnectInfo,
    connectionString,
    defaultConnectInfo,
    fromEnv,
  )
import ZoomHub.Storage.PostgreSQL.Internal
  ( Connection,
    contentImageRowToContent,
    contentRowToContent,
    createConnectionPool,
    deleteImage,
    getBy,
    getBy',
    imageToInsertRow,
    insertContent,
    insertImage,
    markContentAsActive,
    markContentAsFailure,
    markContentAsSuccess,
    resetContentAsInitialized,
    selectContentBy,
    toNominalDiffTime,
  )
import ZoomHub.Storage.PostgreSQL.Schema (Schemas)
import ZoomHub.Types.Content (Content (..))
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentState (ContentState (Active, Initialized))
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.DeepZoomImage (DeepZoomImage)

-- Public API

-- Reads
getById :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentId -> m (Maybe Content)
getById = getBy ((#content ! #hash_id) .== param @1)

getByURL :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentURI -> m (Maybe Content)
getByURL = getBy ((#content ! #url) .== param @1)

getNextUnprocessed :: (MonadUnliftIO m, MonadPQ Schemas m) => m (Maybe Content)
getNextUnprocessed = do
  result <-
    runQueryParams
      ( selectContentBy $
          \t ->
            t
              & where_ ((#content ! #state) .== param @1)
              & orderBy [#content ! #initialized_at & Asc]
              & orderBy [#content ! #num_views & Desc]
              & limit 1
      )
      (Only Initialized)
  contentRow <- firstRow result
  pure (contentImageRowToContent <$> contentRow)

getExpiredActive ::
  (MonadUnliftIO m, MonadPQ Schemas m, TimeUnit t) => t -> m [Content]
getExpiredActive ttl = do
  currentTime <- liftIO getCurrentTime
  let earliestAllowed = addUTCTime (- (toNominalDiffTime ttl)) currentTime
  result <-
    runQueryParams
      ( selectContentBy
          ( \table ->
              table
                & where_
                  ( ((#content ! #active_at) .< param @1)
                      .&& ((#content ! #state) .== literal Active)
                  )
          )
      )
      (Only earliestAllowed)
  contentRows <- getRows result
  return $ contentImageRowToContent <$> contentRows

-- Reads/writes
getById' :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentId -> m (Maybe Content)
getById' = getBy' ((#content ! #hash_id) .== param @1)

getByURL' :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentURI -> m (Maybe Content)
getByURL' = getBy' ((#content ! #url) .== param @1)

-- Writes
initialize ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentURI ->
  Text -> -- Email
  m (Maybe Content)
initialize uri email = do
  verificationToken <- show <$> liftIO UUIDV4.nextRandom
  transactionally_ $ do
    result <- manipulateParams insertContent (uri, Just email, Just verificationToken)
    mRow <- firstRow result
    return $ contentRowToContent <$> mRow

markAsActive ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  m (Maybe Content)
markAsActive cId =
  transactionally_ $ do
    contentResult <- manipulateParams markContentAsActive (Only cId)
    mContentRow <- firstRow contentResult
    return $ contentRowToContent <$> mContentRow

markAsFailure ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  Maybe Text ->
  m (Maybe Content)
markAsFailure cId mErrorMessage =
  transactionally_ $ do
    manipulateParams_ deleteImage (Only cId)
    contentResult <- manipulateParams markContentAsFailure (cId, mErrorMessage)
    mContentRow <- firstRow contentResult
    return $ contentRowToContent <$> mContentRow

markAsSuccess ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  DeepZoomImage ->
  Maybe ContentMIME ->
  Maybe Int64 ->
  m (Maybe Content)
markAsSuccess cId dzi mMIME mSize =
  transactionally_ $ do
    contentResult <- manipulateParams markContentAsSuccess (cId, mMIME, mSize)
    manipulateParams_ insertImage (imageToInsertRow cId dzi)
    mContentRow <- firstRow contentResult
    return $ case mContentRow of
      Just contentRow -> do
        let content = contentRowToContent contentRow
        Just content {contentDZI = Just dzi}
      Nothing ->
        Nothing

resetAsInitialized ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  m (Maybe Content)
resetAsInitialized cId =
  transactionally_ $ do
    contentResult <- manipulateParams resetContentAsInitialized (Only cId)
    mContentRow <- firstRow contentResult
    return $ contentRowToContent <$> mContentRow

dequeueNextUnprocessed ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  m (Maybe Content)
dequeueNextUnprocessed = do
  mNext <- getNextUnprocessed
  case mNext of
    Just next ->
      markAsActive $ contentId next
    Nothing ->
      pure Nothing
