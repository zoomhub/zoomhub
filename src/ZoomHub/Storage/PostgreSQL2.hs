{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module ZoomHub.Storage.PostgreSQL2
  ( -- ** Read operations
    getById
  , create
  ) where

import ZoomHub.Storage.PostgreSQL2.Schema (Schema)
import ZoomHub.Storage.PostgreSQL2.Internal (selectContentBy, rowToContent, insertContent, contentToRow)
import ZoomHub.Types.Content (Content(..))
import ZoomHub.Types.ContentId (ContentId)

import Control.Monad.Trans.Control (MonadBaseControl)
import Squeal.PostgreSQL
  ( MonadPQ
  , Only(..)
  , (!)
  , (.==)
  , firstRow
  , fromOnly
  , getRow
  , manipulateParams
  , param
  , runQueryParams
  )

-- Public API

-- Reads
getById :: (MonadBaseControl IO m, MonadPQ Schema m) => ContentId -> m (Maybe Content)
getById cid = do
  result <- runQueryParams (selectContentBy ((#content ! #hash_id) .== param @1)) (Only cid)
  contentRow <- firstRow result
  pure (rowToContent <$> contentRow)

-- Writes
create :: (MonadBaseControl IO m, MonadPQ Schema m) => Content -> m ContentId
create content = do
  result <- manipulateParams insertContent (contentToRow content)
  fmap fromOnly . getRow 0 $ result
