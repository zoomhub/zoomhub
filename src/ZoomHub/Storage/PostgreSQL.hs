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
    getAllByUserId,
    getNextUnprocessed,
    getExpiredActive,

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
import Control.Monad.Catch (MonadMask)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Units (TimeUnit)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Squeal.PostgreSQL
  ( Inline (inline),
    MonadPQ (executeParams, executeParams_),
    MonadResult (getRows),
    Only (Only),
    SortExpression (Asc, Desc, DescNullsLast),
    firstRow,
    isNotNull,
    limit,
    orderBy,
    param,
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
    createConnectionPool,
    deleteImage,
    getAllBy,
    getBy,
    getBy',
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
import ZoomHub.Types.ContentMIME (ContentMIME)
import qualified ZoomHub.Types.ContentState as ContentState
import ZoomHub.Types.ContentURI (ContentURI (..))
import ZoomHub.Types.DeepZoomImage (DeepZoomImage)
import ZoomHub.Types.VerificationError (VerificationError)
import qualified ZoomHub.Types.VerificationError as VerificationError
import ZoomHub.Types.VerificationToken (VerificationToken)

-- Public API

-- Reads
getById :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentId -> m (Maybe Content)
getById = getBy ((#content ! #hash_id) .== param @1)

getByURL :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentURI -> m (Maybe Content)
getByURL = getBy ((#content ! #url) .== param @1)

getAllByUserId :: (MonadUnliftIO m, MonadPQ Schemas m) => Int64 -> m [Content]
getAllByUserId = getAllBy ((#content ! #user_id) .== param @1)

getNextUnprocessed :: (MonadUnliftIO m, MonadPQ Schemas m) => m (Maybe Content)
getNextUnprocessed = do
  result <-
    executeParams
      ( selectContentBy $
          \table ->
            table
              & where_
                ( (#content ! #state)
                    .== param @1
                    .&& ( #content
                            ! #version
                            .>= 5
                            .&& isNotNull (#content ! #verified_at)
                        )
                )
              & orderBy
                [ #content ! #initialized_at & Asc,
                  #content ! #num_views & Desc
                ]
              & limit 1
      )
      ContentState.Initialized
  firstRow result

getExpiredActive ::
  (MonadUnliftIO m, MonadPQ Schemas m, TimeUnit t) => t -> m [Content]
getExpiredActive ttl = do
  currentTime <- liftIO getCurrentTime
  let earliestAllowed = addUTCTime (-(toNominalDiffTime ttl)) currentTime
  result <-
    executeParams
      ( selectContentBy
          ( \table ->
              table
                & where_
                  ( ((#content ! #active_at) .< param @1)
                      .&& ((#content ! #state) .== inline ContentState.Active)
                  )
                & orderBy [#content ! #active_at & DescNullsLast]
          )
      )
      earliestAllowed
  getRows result

-- Reads/writes
getById' :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentId -> m (Maybe Content)
getById' = getBy' ((#content ! #hash_id) .== param @1)

getByURL' :: (MonadUnliftIO m, MonadPQ Schemas m) => ContentURI -> m (Maybe Content)
getByURL' = getBy' ((#content ! #url) .== param @1)

-- Writes
initialize ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  ContentURI ->
  Text -> -- Email
  m (Maybe Content)
initialize uri email = do
  verificationToken <- UUID.toText <$> liftIO UUIDV4.nextRandom
  transactionally_ $ do
    result <-
      executeParams
        insertContent
        ( uri,
          Just email,
          Just verificationToken
        )
    firstRow result

markAsActive ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  ContentId ->
  m (Maybe Content)
markAsActive cId =
  transactionally_ $ do
    result <- executeParams markContentAsActive (Only cId)
    firstRow result

markAsFailure ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  ContentId ->
  Maybe Text ->
  m (Maybe Content)
markAsFailure cId mErrorMessage =
  transactionally_ $ do
    executeParams_ deleteImage (Only cId)
    result <- executeParams markContentAsFailure (cId, mErrorMessage)
    firstRow result

markAsSuccess ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  ContentId ->
  DeepZoomImage ->
  Maybe ContentMIME ->
  Maybe Int64 ->
  m (Maybe Content)
markAsSuccess cId dzi mMIME mSize =
  transactionally_ $ do
    result <- executeParams markContentAsSuccess (cId, mMIME, mSize)
    executeParams_ insertImage (cId, dzi)
    mContent <- firstRow result
    return $ case mContent of
      Just content -> do
        Just content {contentDZI = Just dzi}
      Nothing ->
        Nothing

markAsVerified ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  ContentId ->
  VerificationToken ->
  m (Either VerificationError Content)
markAsVerified cId verificationToken = do
  mContent <- getById cId
  transactionally_ $ do
    case mContent >>= contentVerificationToken of
      Just token | token == verificationToken -> do
        contentResult <- executeParams markContentAsVerified (Only cId)
        mContent' <- firstRow contentResult
        case mContent' of
          Just content ->
            return $ Right content
          Nothing ->
            return $ Left VerificationError.ContentNotFound
      Just _ ->
        return $ Left VerificationError.TokenMismatch
      Nothing ->
        return $ Left VerificationError.ContentNotFound

resetAsInitialized ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  ContentId ->
  m (Maybe Content)
resetAsInitialized cId =
  transactionally_ $ do
    executeParams_ deleteImage (Only cId)
    contentResult <- executeParams resetContentAsInitialized (Only cId)
    firstRow contentResult

unsafeResetAsInitializedWithVerification ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  ContentId ->
  m (Maybe Content)
unsafeResetAsInitializedWithVerification cId =
  transactionally_ $ do
    executeParams_ deleteImage (Only cId)
    void $ executeParams resetContentAsInitialized (Only cId)
    contentResult <- executeParams markContentAsVerified (Only cId)
    firstRow contentResult

dequeueNextUnprocessed ::
  (MonadUnliftIO m, MonadPQ Schemas m, MonadMask m) =>
  m (Maybe Content)
dequeueNextUnprocessed = do
  mNext <- getNextUnprocessed
  case mNext of
    Just next ->
      markAsActive $ contentId next
    Nothing ->
      pure Nothing
