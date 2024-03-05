{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.GetRecent
  ( getRecent,
  )
where

import Data.Binary (Word64)
import Squeal.PostgreSQL
  ( MonadPQ (executeParams),
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
  ( selectContentBy,
  )
import ZoomHub.Storage.PostgreSQL.Schema (Schemas)
import ZoomHub.Types.Content (Content (..))

getRecent :: (MonadUnliftIO m, MonadPQ Schemas m) => Word64 -> m [Content]
getRecent numItems = do
  result <-
    executeParams
      ( selectContentBy
          ( \table ->
              table
                -- TODO: Remove dummy clause required to pass in parameter
                & where_ ((#content ! #hash_id) ./= param @1)
                & orderBy [#content ! #initialized_at & Desc]
                & limit numItems
          )
      )
      (Only "1")
  getRows result
