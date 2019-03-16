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
  , create
  ) where

import ZoomHub.Storage.PostgreSQL2.Internal
  ( InsertContentResult(..)
  , contentToRow
  , createImage
  , getBy
  , getBy'
  , insertContent
  )
import ZoomHub.Storage.PostgreSQL2.Schema (Schema)
import ZoomHub.Types.Content (Content(..))
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentURI (ContentURI)

import Control.Monad (void)
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Time.Clock (getCurrentTime)
import Squeal.PostgreSQL
  (MonadPQ, getRow, manipulateParams, param, transactionally_, (!), (.==))

-- Public API

-- Reads
getById :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentId -> m (Maybe Content)
getById cid = getBy ((#content ! #hash_id) .== param @1) cid

getByURL :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentURI -> m (Maybe Content)
getByURL url = getBy ((#content ! #url) .== param @1) url

-- Reads/writes
getById' :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentId -> m (Maybe Content)
getById' cid = getBy' ((#content ! #hash_id) .== param @1) cid

getByURL' :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentURI -> m (Maybe Content)
getByURL' url = getBy' ((#content ! #url) .== param @1) url

-- Writes
-- TODO: Make this function total
create :: (MonadBaseControl IO m, MonadPQ Schema m) => Content -> m ContentId
create content =
  transactionally_ $ do
    result <- manipulateParams insertContent (contentToRow content)
    row <- getRow 0 result
    case contentDZI content of
      (Just dzi) -> do
        initializedAt <- liftBase getCurrentTime
        void $ createImage (icrId row) initializedAt dzi
      Nothing ->
        return ()
    return (icrHashId row)
