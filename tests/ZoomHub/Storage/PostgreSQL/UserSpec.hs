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
    focus,
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
import ZoomHub.Types.User (Email (Email))
import qualified ZoomHub.Types.User as User

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  beforeAll_ setupDatabase $ around withDatabaseConnection do
    focus $ describe "linkVerifiedContent" do
      it "should return verified content that matches email of user" do
        \conn -> do
          let canonicalEmail = Email (CI.mk "user-1@example.com")
          let alternativeEmail = Email (CI.mk "USER-1@example.com")
          let mismatchingEmail = Email (CI.mk "user-2@example.com")

          (canonicalUser, _) <- runPQ (PGUser.findOrCreate (mkCreateUser canonicalEmail)) conn
          -- TODO: Test that this doesn’t create a different user, i.e. use `citext`:
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
