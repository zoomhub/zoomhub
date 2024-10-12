{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.Dashboard
  ( getByEmail,
  )
where

import Data.Text (Text)
import Squeal.PostgreSQL
  ( MonadPQ (executeParams),
    SortExpression (Desc),
    getRows,
    limit,
    orderBy,
    param,
    where_,
    (!),
    (&),
    (.==),
  )
import UnliftIO (MonadUnliftIO)
import ZoomHub.Storage.PostgreSQL.Internal
  ( selectContentBy,
  )
import ZoomHub.Storage.PostgreSQL.Schema (Schemas)
import ZoomHub.Types.Content (Content (..))

getByEmail :: (MonadUnliftIO m, MonadPQ Schemas m) => Text -> m [Content]
getByEmail email = do
  result <-
    executeParams
      ( selectContentBy
          ( \table ->
              table
                & where_ ((#content ! #submitter_email) .== param @1)
                & orderBy [#content ! #initialized_at & Desc]
                & limit numItems
          )
      )
      email
  getRows result
  where
    numItems = 10
