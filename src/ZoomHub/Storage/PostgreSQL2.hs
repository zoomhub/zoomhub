{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module ZoomHub.Storage.PostgreSQL2
  ( -- ** Read operations
    getById
  , create
  ) where

import ZoomHub.Storage.PostgreSQL2.Internal
  ( InsertContentResult(..)
  , contentToRow
  , createImage
  , insertContent
  , rowToContent
  , selectContentBy
  )
import ZoomHub.Storage.PostgreSQL2.Schema (Schema)
import ZoomHub.Types.Content (Content(..))
import ZoomHub.Types.ContentId (ContentId)

import Control.Monad (void)
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Time.Clock (getCurrentTime)
import Squeal.PostgreSQL
  ( MonadPQ
  , Only(..)
  , firstRow
  , getRow
  , manipulateParams
  , param
  , runQueryParams
  , transactionally_
  , (!)
  , (.==)
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
