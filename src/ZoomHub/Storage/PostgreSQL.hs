{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module ZoomHub.Storage.PostgreSQL
  ( Schemas
  , Connection
  , createConnectionPool
    -- ** Connection
  , ConnectInfo(..)
  , defaultConnectInfo
  , fromEnv
  , postgreSQLConnectionString
    -- ** Read operations
  , getById
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
  , resetAsInitialized
  , dequeueNextUnprocessed
  ) where

import ZoomHub.Storage.PostgreSQL.Internal
  ( Connection
  , contentImageRowToContent
  , contentRowToContent
  , createConnectionPool
  , getBy
  , getBy'
  , imageToInsertRow
  , insertContent
  , insertImage
  , markContentAsActive
  , markContentAsFailure
  , markContentAsSuccess
  , resetContentAsInitialized
  , selectContentBy
  , toNominalDiffTime
  )
import ZoomHub.Storage.PostgreSQL.Schema (Schemas)
import ZoomHub.Types.Content (Content(..))
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentState (ContentState(Initialized))
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.DeepZoomImage (DeepZoomImage)

import Control.Monad (void)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Units (TimeUnit)
import Database.PostgreSQL.Simple
  (ConnectInfo(..), defaultConnectInfo, postgreSQLConnectionString)
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
import System.Environment (getEnvironment)
import Text.Read (readMaybe)
import UnliftIO (MonadUnliftIO, liftIO)

-- Public API

-- Reads
getById :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentId -> m (Maybe Content)
getById = getBy ((#content ! #hash_id) .== param @1)

getByURL :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentURI -> m (Maybe Content)
getByURL = getBy ((#content ! #url) .== param @1)

getNextUnprocessed :: (MonadUnliftIO m, MonadPQ Schemas m) => m (Maybe Content)
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
  (MonadUnliftIO m, MonadPQ Schemas m, TimeUnit t) => t -> m [Content]
getExpiredActive ttl = do
  currentTime <- liftIO getCurrentTime
  let earliestAllowed = addUTCTime (-(toNominalDiffTime ttl)) currentTime
  result <- runQueryParams
    (selectContentBy
      (\t -> t & where_ ((#content ! #active_at) .< param @1))
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
initialize
  :: (MonadUnliftIO m, MonadPQ Schemas m)
  => ContentURI
  -> m (Maybe Content)
initialize uri =
  transactionally_ $ do
    result <- manipulateParams insertContent (Only uri)
    mRow <- firstRow result
    return $ contentRowToContent <$> mRow

markAsActive
  :: (MonadUnliftIO m, MonadPQ Schemas m)
  => ContentId
  -> m (Maybe Content)
markAsActive cId =
  transactionally_ $ do
    contentResult <- manipulateParams markContentAsActive (Only cId)
    mContentRow <- firstRow contentResult
    return $ contentRowToContent <$> mContentRow

markAsFailure
  :: (MonadUnliftIO m, MonadPQ Schemas m)
  => ContentId
  -> Maybe Text
  -> m (Maybe Content)
markAsFailure cId mErrorMessage =
  transactionally_ $ do
    contentResult <- manipulateParams markContentAsFailure (cId, mErrorMessage)
    mContentRow <- firstRow contentResult
    return $ contentRowToContent <$> mContentRow

markAsSuccess
  :: (MonadUnliftIO m, MonadPQ Schemas m)
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

resetAsInitialized
  :: (MonadUnliftIO m, MonadPQ Schemas m)
  => ContentId
  -> m (Maybe Content)
resetAsInitialized cId =
  transactionally_ $ do
    contentResult <- manipulateParams resetContentAsInitialized (Only cId)
    mContentRow <- firstRow contentResult
    return $ contentRowToContent <$> mContentRow

dequeueNextUnprocessed
  :: (MonadUnliftIO m, MonadPQ Schemas m)
  => m (Maybe Content)
dequeueNextUnprocessed = do
  mNext <- getNextUnprocessed
  case mNext of
    Just next ->
      markAsActive $ contentId next
    Nothing ->
      pure Nothing

fromEnv :: String -> IO ConnectInfo
fromEnv dbName = do
  env <- getEnvironment

  let defaultDBHost = connectHost defaultConnectInfo
      defaultDBPort = connectPort defaultConnectInfo
      defaultDBUser = connectUser defaultConnectInfo
      defaultDBPassword = connectPassword defaultConnectInfo

  return $ ConnectInfo
    { connectHost = fromMaybe defaultDBHost (lookup "PGHOST" env)
    , connectPort = fromMaybe defaultDBPort (lookup "PGPORT" env >>= readMaybe)
    , connectUser = fromMaybe defaultDBUser (lookup "PGUSER" env)
    , connectPassword = fromMaybe defaultDBPassword (lookup "PGPASSWORD" env)
    , connectDatabase = fromMaybe dbName (lookup "PGDATABASE" env)
    }
