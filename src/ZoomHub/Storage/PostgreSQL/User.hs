{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.User
  ( findOrCreate,
    linkVerifiedContent,
    Internal.CreateUser (..),
  )
where

import Control.Monad.Catch (MonadMask)
import Data.CaseInsensitive (CI)
import Data.Text (Text)
import Squeal.PostgreSQL
  ( MonadPQ (executeParams),
    Only (Only),
    firstRow,
  )
import Squeal.PostgreSQL.Session.Monad (MonadPQ (executeParams_))
import UnliftIO (MonadUnliftIO)
import qualified ZoomHub.Storage.PostgreSQL.Internal.User as Internal
import ZoomHub.Storage.PostgreSQL.Schema (Schemas)
import ZoomHub.Types.User (User)

findOrCreate ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  Internal.CreateUser ->
  m User
findOrCreate user = do
  result <- executeParams Internal.findOrCreate user
  mUser <- firstRow result
  case mUser of
    Nothing -> error "Unexpected error: Could not create user with `findOrCreate`"
    Just user' -> pure user'

linkVerifiedContent ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) => CI Text -> m ()
linkVerifiedContent email =
  executeParams_ Internal.linkVerifiedContent (Only email)
