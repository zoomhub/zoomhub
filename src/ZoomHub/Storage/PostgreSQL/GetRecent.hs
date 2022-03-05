{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.GetRecent
  ( getRecent,
  )
where

import Data.Binary (Word64)
import Squeal.PostgreSQL
  ( MonadPQ,
    Only (Only),
    SortExpression (Desc),
    getRows,
    limit,
    orderBy,
    param,
    runQueryParams,
    where_,
    (!),
    (&),
    (./=),
  )
import UnliftIO (MonadUnliftIO)
import ZoomHub.Storage.PostgreSQL.Internal
  ( contentImageRowToContent,
    selectContentBy,
  )
import ZoomHub.Storage.PostgreSQL.Schema (Schemas)
import ZoomHub.Types.Content (Content (..))
import ZoomHub.Types.ContentState (ContentState (Active))

getRecent :: (MonadUnliftIO m, MonadPQ Schemas m) => Word64 -> m [Content]
getRecent numItems = do
  result <-
    runQueryParams
      ( selectContentBy
          ( \table ->
              table
                & where_ ((#content ! #state) ./= param @1) -- TODO: Remove dummy clause
                & orderBy [#content ! #initialized_at & Desc]
                & limit numItems
          )
      )
      (Only Active)

  contentRows <- getRows result
  return $ contentImageRowToContent <$> contentRows
