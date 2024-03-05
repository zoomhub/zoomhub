{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL
  ( -- ** Schema
    Schemas,

    -- ** Connection
    Connection,
    createConnectionPool,

    -- ** Read operations
    getById,
    getByURL,
    getNextUnprocessed,

    -- ** Read operations (with view tracking)
    getById',
    getByURL',

    -- ** Write operations
    initialize,
    markAsActive,
    markAsFailure,
    markAsSuccess,
    markAsVerified,
    resetAsInitialized,
    unsafeResetAsInitializedWithVerification,
    dequeueNextUnprocessed,
    module ZoomHub.Storage.PostgreSQL.ConnectInfo,
  )
where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Units (TimeUnit)
import qualified Data.UUID.V4 as UUIDV4
import Squeal.PostgreSQL
  ( MonadPQ (executeParams),
    Only (Only),
    SortExpression (Asc, Desc, DescNullsLast),
    firstRow,
    getRows,
    inline,
    isNotNull,
    limit,
    manipulateParams,
    manipulateParams_,
    orderBy,
    param,
    runQueryParams,
    transactionally_,
    where_,
    (!),
    (&),
    (.&&),
    (.<),
    (.==),
    (.>=),
  )
import UnliftIO (MonadUnliftIO, liftIO)
import ZoomHub.Storage.PostgreSQL.ConnectInfo
  ( ConnectInfo,
    connectionString,
    defaultConnectInfo,
    fromEnv,
  )
import ZoomHub.Storage.PostgreSQL.Internal
  ( Connection,
    contentRowToContent,
    createConnectionPool,
    deleteImage,
    getBy,
    getBy',
    imageToInsertRow,
    insertContent,
    insertImage,
    markContentAsActive,
    markContentAsFailure,
    markContentAsSuccess,
    markContentAsVerified,
    resetContentAsInitialized,
    selectContentBy,
    toNominalDiffTime,
  )
import ZoomHub.Storage.PostgreSQL.Schema (Schemas)
import ZoomHub.Types.Content (Content (..))
import ZoomHub.Types.ContentId (ContentId)
import qualified ZoomHub.Types.ContentId as ContentId
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentState (ContentState (Active, Initialized))
import qualified ZoomHub.Types.ContentState as ContentState
import ZoomHub.Types.ContentURI (ContentURI (..))
import qualified ZoomHub.Types.ContentURI as ContentURI
import ZoomHub.Types.DeepZoomImage (DeepZoomImage)
import ZoomHub.Types.VerificationError (VerificationError)
import qualified ZoomHub.Types.VerificationError as VerificationError
import ZoomHub.Types.VerificationToken (VerificationToken)

-- Public API

-- Reads
getById :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentId -> m (Maybe Content)
getById id_ = getBy ((#content ! #hash_id) .== param @1) (ContentId.toText id_)

getByURL :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentURI -> m (Maybe Content)
getByURL uri = getBy ((#content ! #url) .== param @1) (unContentURI uri)

{- ORMOLU_DISABLE -}
getNextUnprocessed :: (MonadUnliftIO m, MonadPQ Schemas m) => m (Maybe Content)
getNextUnprocessed = do
  result <- executeParams
      ( selectContentBy $
          \table ->
            table
              & where_
                ( (#content ! #state) .== param @1
                  .&& ( #content ! #version .>= 5
                        .&& isNotNull (#content ! #verified_at)
                      )
                )
              & orderBy
                [ #content ! #initialized_at & Asc,
                  #content ! #num_views & Desc
                ]
              & limit 1
      )
      -- TODO: Remove text conversion once `selectContentBy` is generic:
      (Only (Initialized & ContentState.toText))
  firstRow result
{- ORMOLU_ENABLE -}

-- Reads/writes
getById' :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentId -> m (Maybe Content)
getById' id_ = getBy' ((#content ! #hash_id) .== param @1) (ContentId.toText id_)

getByURL' :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentURI -> m (Maybe Content)
getByURL' uri = getBy' ((#content ! #url) .== param @1) (unContentURI uri)

-- Writes
initialize ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentURI ->
  Text -> -- Email
  m (Maybe Content)
initialize _uri _email = pure Nothing

-- initialize uri email = do
--   verificationToken <- show <$> liftIO UUIDV4.nextRandom
--   transactionally_ $ do
--     result <-
--       manipulateParams
--         insertContent
--         ( uri,
--           Just email,
--           Just verificationToken
--         )
--     mRow <- firstRow result
--     return $ contentRowToContent <$> mRow

markAsActive ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  m (Maybe Content)
markAsActive _cId = pure Nothing

-- markAsActive cId =
--   transactionally_ $ do
--     contentResult <- manipulateParams markContentAsActive (Only cId)
--     mContentRow <- firstRow contentResult
--     return $ contentRowToContent <$> mContentRow

markAsFailure ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  Maybe Text ->
  m (Maybe Content)
markAsFailure _cId _mErrorMessage = pure Nothing

-- markAsFailure cId mErrorMessage =
--   transactionally_ $ do
--     manipulateParams_ deleteImage (Only cId)
--     contentResult <- manipulateParams markContentAsFailure (cId, mErrorMessage)
--     mContentRow <- firstRow contentResult
--     return $ contentRowToContent <$> mContentRow

markAsSuccess ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  DeepZoomImage ->
  Maybe ContentMIME ->
  Maybe Int64 ->
  m (Maybe Content)
markAsSuccess cId dzi mMIME mSize = pure Nothing

-- markAsSuccess cId dzi mMIME mSize =
--   transactionally_ $ do
--     contentResult <- manipulateParams markContentAsSuccess (cId, mMIME, mSize)
--     manipulateParams_ insertImage (imageToInsertRow cId dzi)
--     mContentRow <- firstRow contentResult
--     return $ case mContentRow of
--       Just contentRow -> do
--         let content = contentRowToContent contentRow
--         Just content {contentDZI = Just dzi}
--       Nothing ->
--         Nothing

markAsVerified ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  VerificationToken ->
  m (Either VerificationError Content)
markAsVerified cId verificationToken = pure $ Left VerificationError.ContentNotFound

-- markAsVerified cId verificationToken =
--   transactionally_ $ do
--     mContent <- getById cId
--     case mContent >>= contentVerificationToken of
--       Just token | token == verificationToken -> do
--         contentResult <- manipulateParams markContentAsVerified (Only cId)
--         mContentRow <- firstRow contentResult
--         case mContentRow of
--           Just contentRow ->
--             return $ Right $ contentRowToContent contentRow
--           Nothing ->
--             return $ Left VerificationError.ContentNotFound
--       Just _ ->
--         return $ Left VerificationError.TokenMismatch
--       Nothing ->
--         return $ Left VerificationError.ContentNotFound

resetAsInitialized ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  m (Maybe Content)
resetAsInitialized _cId = pure Nothing

-- resetAsInitialized cId =
--   transactionally_ $ do
--     manipulateParams_ deleteImage (Only cId)
--     contentResult <- manipulateParams resetContentAsInitialized (Only cId)
--     mContentRow <- firstRow contentResult
--     return $ contentRowToContent <$> mContentRow

unsafeResetAsInitializedWithVerification ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  ContentId ->
  m (Maybe Content)
unsafeResetAsInitializedWithVerification _cId = pure Nothing

-- unsafeResetAsInitializedWithVerification cId =
--   transactionally_ $ do
--     manipulateParams_ deleteImage (Only cId)
--     void $ manipulateParams resetContentAsInitialized (Only cId)
--     contentResult <- manipulateParams markContentAsVerified (Only cId)
--     mContentRow <- firstRow contentResult
--     return $ contentRowToContent <$> mContentRow

dequeueNextUnprocessed ::
  (MonadUnliftIO m, MonadPQ Schemas m) =>
  m (Maybe Content)
dequeueNextUnprocessed = pure Nothing

-- dequeueNextUnprocessed = do
--   mNext <- getNextUnprocessed
--   case mNext of
--     Just next ->
--       markAsActive $ contentId next
--     Nothing ->
--       pure Nothing
