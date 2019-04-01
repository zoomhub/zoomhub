{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module ZoomHub.Storage.PostgreSQL2
  ( -- ** Read operations
    getById
  , getByURL
    -- ** Read/write operations
  , getById'
  , getByURL'
    -- ** Write operations
  , initialize
  , markAsSuccess
  ) where

import ZoomHub.Storage.PostgreSQL2.Internal
  ( getBy
  , getBy'
  , imageToInsertRow
  , insertContent
  , insertContentResultToContent
  , insertImage
  , markContentAsSuccess
  )
import ZoomHub.Storage.PostgreSQL2.Schema (Schema)
import ZoomHub.Types.Content (Content(..))
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.DeepZoomImage (DeepZoomImage)

import Control.Monad (void)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Int (Int64)
import Squeal.PostgreSQL
  ( MonadPQ
  , Only(..)
  , firstRow
  , manipulateParams
  , param
  , transactionally_
  , (!)
  , (.==)
  )

-- Public API

-- Reads
getById :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentId -> m (Maybe Content)
getById = getBy ((#content ! #hash_id) .== param @1)

getByURL :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentURI -> m (Maybe Content)
getByURL = getBy ((#content ! #url) .== param @1)

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
    return $ insertContentResultToContent <$> mRow

-- TODO: Return updated content
markAsSuccess
  :: (MonadBaseControl IO m, MonadPQ Schema m)
  => ContentId
  -> DeepZoomImage
  -> Maybe ContentMIME
  -> Maybe Int64
  -> m ()
markAsSuccess cId dzi mMIME mSize =
  transactionally_ $ do
    void $ manipulateParams markContentAsSuccess (cId, mMIME, mSize)
    void $ manipulateParams insertImage (imageToInsertRow cId dzi)
