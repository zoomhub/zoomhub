{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}

module ZoomHub.Storage.PostgreSQL.UserSpec
  ( main,
    spec,
  )
where

import Control.Monad (forM_, void)
import qualified Data.CaseInsensitive as CI
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Text (Text)
import Flow
import Squeal.PostgreSQL
  ( runPQ,
  )
import Test.Hspec
  ( Spec,
    around,
    beforeAll_,
    describe,
    expectationFailure,
    hspec,
    it,
    shouldBe,
  )
import ZoomHub.Storage.PostgreSQL (getAllByUserId)
import qualified ZoomHub.Storage.PostgreSQL.Internal as I
import qualified ZoomHub.Storage.PostgreSQL.User as CreateUser
import qualified ZoomHub.Storage.PostgreSQL.User as PGUser
import ZoomHub.Storage.PostgreSQLSpec (mkSucceededContent, safeGetCurrentTime, setupDatabase, withDatabaseConnection)
import ZoomHub.Types.Content (Content (contentId, contentSubmitterEmail, contentUserId))
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.User (Email (Email), UserCreationResult (UserCreated, UserFound))
import qualified ZoomHub.Types.User as User

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  beforeAll_ setupDatabase $ around withDatabaseConnection do
    describe "findByEmail" do
      it "should return existing user when email matches" do
        \conn -> do
          let testEmail = CI.mk "test@example.com"
          let createUser = mkCreateUserFromEmail testEmail

          -- Create a user first
          (createdUser, _) <- runPQ (PGUser.findOrCreate createUser) conn

          -- Find the user by email
          (foundUser, _) <- runPQ (PGUser.findByEmail testEmail) conn

          foundUser `shouldBe` Just createdUser

      it "should return Nothing when email does not exist" do
        \conn -> do
          let nonExistentEmail = CI.mk "nonexistent@example.com"

          (foundUser, _) <- runPQ (PGUser.findByEmail nonExistentEmail) conn

          foundUser `shouldBe` Nothing

      it "should find user with case-insensitive email matching" do
        \conn -> do
          let originalEmail = CI.mk "CaseTest@Example.COM"
          let searchEmail = CI.mk "casetest@example.com"
          let createUser = mkCreateUserFromEmail originalEmail

          -- Create user with mixed case email
          (createdUser, _) <- runPQ (PGUser.findOrCreate createUser) conn

          -- Search with different case
          (foundUser, _) <- runPQ (PGUser.findByEmail searchEmail) conn

          foundUser `shouldBe` Just createdUser

    describe "findOrCreateWithResult" do
      it "should return UserCreated when creating new user" do
        \conn -> do
          let testEmail = CI.mk "newuser@example.com"
          let createUser = mkCreateUserFromEmail testEmail

          (userResult, _) <- runPQ (PGUser.findOrCreateWithResult createUser) conn

          case userResult of
            UserCreated user -> do
              user.email `shouldBe` testEmail
            UserFound _ ->
              expectationFailure "Expected UserCreated, got UserFound"

      it "should return UserFound when user already exists" do
        \conn -> do
          let testEmail = CI.mk "existinguser@example.com"
          let createUser = mkCreateUserFromEmail testEmail

          -- Create user first
          (firstResult, _) <- runPQ (PGUser.findOrCreateWithResult createUser) conn
          firstUserId <- case firstResult of
            UserCreated user -> pure user.id
            UserFound _ -> do
              expectationFailure "Expected UserCreated on first call"
              pure 0 -- This won't be reached but satisfies type checker

          -- Call again - should find existing user
          (secondResult, _) <- runPQ (PGUser.findOrCreateWithResult createUser) conn
          case secondResult of
            UserFound user -> do
              user.id `shouldBe` firstUserId
              user.email `shouldBe` testEmail
            UserCreated _ ->
              expectationFailure "Expected UserFound, got UserCreated"

    describe "linkVerifiedContent" do
      it "should return verified content that matches email of user" do
        \conn -> do
          let canonicalEmail = Email (CI.mk "user-1@example.com")
          let alternativeEmail = Email (CI.mk "USER-1@example.com")
          let mismatchingEmail = Email (CI.mk "user-2@example.com")

          (canonicalUser, _) <- runPQ (PGUser.findOrCreate (mkCreateUser canonicalEmail)) conn
          (alternativeUser, _) <-
            runPQ (PGUser.findOrCreate (mkCreateUser alternativeEmail)) conn
          void $
            runPQ (PGUser.findOrCreate (mkCreateUser mismatchingEmail)) conn

          currentTime <- safeGetCurrentTime
          let minutes n = n * 60
              -- HACK: Hard-coded content IDs set by database trigger
              c1 = mkSucceededContent "X75" (Just canonicalEmail) currentTime (15 & minutes)
              c2 = mkSucceededContent "yOJ" (Just alternativeEmail) currentTime (45 & minutes)
              c3 = mkSucceededContent "yJL" (Just mismatchingEmail) currentTime (60 & minutes)
          void . forM_ [c1, c2, c3] $ \c ->
            runPQ (I.unsafeCreateContent c) conn
          let (Email canonicalEmailCI) = canonicalEmail
          void $ runPQ (PGUser.linkVerifiedContent canonicalEmailCI) conn
          (results, _) <- runPQ (getAllByUserId canonicalUser.id) conn
          (results <&> toTuple)
            `shouldBe` ( [ c1
                             { contentUserId = Just canonicalUser.id,
                               contentSubmitterEmail = Nothing
                             },
                           c2
                             { contentUserId = Just canonicalUser.id,
                               contentSubmitterEmail = Nothing
                             }
                         ]
                           <&> toTuple
                       )
  where
    toTuple :: Content -> (ContentId, Maybe Text, Maybe Int64)
    toTuple content =
      ( content |> contentId,
        content |> contentSubmitterEmail,
        content |> contentUserId
      )

    mkCreateUser :: Email -> PGUser.CreateUser
    mkCreateUser (Email ciEmail) =
      PGUser.CreateUser
        { CreateUser.kindeUserId = "kp_" <> CI.original ciEmail,
          CreateUser.email = ciEmail,
          CreateUser.isEmailVerified = True,
          CreateUser.givenName = Nothing,
          CreateUser.familyName = Nothing,
          CreateUser.imageURL = Nothing
        }

    mkCreateUserFromEmail :: CI.CI Text -> PGUser.CreateUser
    mkCreateUserFromEmail ciEmail =
      PGUser.CreateUser
        { CreateUser.kindeUserId = "kp_" <> CI.original ciEmail,
          CreateUser.email = ciEmail,
          CreateUser.isEmailVerified = True,
          CreateUser.givenName = Nothing,
          CreateUser.familyName = Nothing,
          CreateUser.imageURL = Nothing
        }
