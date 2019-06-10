{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module ZoomHub.Storage.PostgreSQL2
  ( -- ** Read operations
    getById
  , getByURL
  , getNextUnprocessed
  , getExpiredActive
    -- ** Read operations (with view tracking)
  , getById'
  , getByURL'
    -- ** Write operations
  , initialize
  , markAsActive
  , markAsFailure
  , markAsSuccess
  ) where

import ZoomHub.Storage.PostgreSQL2.Internal
  ( contentImageRowToContent
  , contentRowToContent
  , getBy
  , getBy'
  , imageToInsertRow
  , insertContent
  , insertImage
  , markContentAsActive
  , markContentAsFailure
  , markContentAsSuccess
  , selectContentBy
  )
import ZoomHub.Storage.PostgreSQL2.Schema (Schema)
import ZoomHub.Types.Content (Content(..))
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentState (ContentState(Initialized))
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.DeepZoomImage (DeepZoomImage)

import Control.Monad (void)
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Time.Units (Second, TimeUnit, toMicroseconds)
import Squeal.PostgreSQL
  ( MonadPQ
  , Only(Only)
  , SortExpression(Asc, Desc)
  , firstRow
  , getRows
  , limit
  , manipulateParams
  , orderBy
  , param
  , runQueryParams
  , transactionally_
  , where_
  , (!)
  , (&)
  , (.<)
  , (.==)
  )

-- Public API

-- Reads
getById :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentId -> m (Maybe Content)
getById = getBy ((#content ! #hash_id) .== param @1)

getByURL :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentURI -> m (Maybe Content)
getByURL = getBy ((#content ! #url) .== param @1)

getNextUnprocessed :: (MonadBaseControl IO m, MonadPQ Schema m) => m (Maybe Content)
getNextUnprocessed = do
  result <- runQueryParams
    (selectContentBy $
      \t -> t
        & where_ ((#content ! #state) .== param @1)
        & orderBy [#content ! #initialized_at & Asc]
        & orderBy [#content ! #num_views & Desc]
        & limit 1
    ) (Only Initialized)
  contentRow <- firstRow result
  pure (contentImageRowToContent <$> contentRow)

getExpiredActive ::
  (MonadBaseControl IO m, MonadPQ Schema m, TimeUnit t) => t -> m [Content]
getExpiredActive ttl = do
  currentTime <- liftBase getCurrentTime
  let earliestAllowed = addUTCTime (-(toNominalDiffTime ttl)) currentTime
  result <- runQueryParams
    (selectContentBy
      (\t -> t & where_ ((#content ! #active_at) .< param @1))
    )
    (Only earliestAllowed)
  contentRows <- getRows result
  return $ contentImageRowToContent <$> contentRows
  where
  toNominalDiffTime :: (TimeUnit a) => a -> NominalDiffTime
  toNominalDiffTime duration = fromIntegral $
    toMicroseconds duration `div` toMicroseconds (1 :: Second)

-- Reads/writes
getById' :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentId -> m (Maybe Content)
getById' = getBy' ((#content ! #hash_id) .== param @1)

getByURL' :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentURI -> m (Maybe Content)
getByURL' = getBy' ((#content ! #url) .== param @1)

-- Writes
initialize
  :: (MonadBaseControl IO m, MonadPQ Schema m)
  => ContentURI
  -> m (Maybe Content)
initialize uri =
  transactionally_ $ do
    result <- manipulateParams insertContent (Only uri)
    mRow <- firstRow result
    return $ contentRowToContent <$> mRow

markAsActive
  :: (MonadBaseControl IO m, MonadPQ Schema m)
  => ContentId
  -> m (Maybe Content)
markAsActive cId =
  transactionally_ $ do
    contentResult <- manipulateParams markContentAsActive (Only cId)
    mContentRow <- firstRow contentResult
    return $ contentRowToContent <$> mContentRow

markAsFailure
  :: (MonadBaseControl IO m, MonadPQ Schema m)
  => ContentId
  -> Maybe Text
  -> m (Maybe Content)
markAsFailure cId mErrorMessage =
  transactionally_ $ do
    contentResult <- manipulateParams markContentAsFailure (cId, mErrorMessage)
    mContentRow <- firstRow contentResult
    return $ contentRowToContent <$> mContentRow

markAsSuccess
  :: (MonadBaseControl IO m, MonadPQ Schema m)
  => ContentId
  -> DeepZoomImage
  -> Maybe ContentMIME
  -> Maybe Int64
  -> m (Maybe Content)
markAsSuccess cId dzi mMIME mSize =
  transactionally_ $ do
    contentResult <- manipulateParams markContentAsSuccess (cId, mMIME, mSize)
    void $ manipulateParams insertImage (imageToInsertRow cId dzi)
    mContentRow <- firstRow contentResult
    return $ case mContentRow of
      Just contentRow -> do
        let content = contentRowToContent contentRow
        Just content { contentDZI = Just dzi }
      Nothing ->
        Nothing
