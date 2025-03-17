{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}

module ZoomHub.Storage.PostgreSQL.UserSpec
  ( main,
    spec,
  )
where

import Control.Monad (forM_, void)
import Data.Function ((&))
import Data.Time.Units (Minute)
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
import ZoomHub.Storage.PostgreSQL (getExpiredActive)
import qualified ZoomHub.Storage.PostgreSQL.Internal as I
import qualified ZoomHub.Storage.PostgreSQL.User as User
import ZoomHub.Storage.PostgreSQLSpec (mkSucceededContent, safeGetCurrentTime, setupDatabase, withDatabaseConnection)
import ZoomHub.Types.User (Email (Email))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  beforeAll_ setupDatabase $ around withDatabaseConnection do
    focus $ describe "linkVerifiedContent" do
      it "should return verified content that matches email of user" do
        \conn -> do
          let canonicalEmail = Email "user-1@example.com"
          let alternativeEmail = Email "USER-1@example.com"
          let mismatchingEmail = Email "user-2@example.com"
          currentTime <- safeGetCurrentTime
          let minutes n = n * 60
              -- HACK: Hard-coded content IDs set by database trigger
              c1 = mkSucceededContent "X75" (Just canonicalEmail) currentTime (15 & minutes)
              c2 = mkSucceededContent "yOJ" (Just alternativeEmail) currentTime (45 & minutes)
              c3 = mkSucceededContent "yJL" (Just mismatchingEmail) currentTime (60 & minutes)
          void . forM_ [c1, c2, c3] $ \c ->
            runPQ (I.unsafeCreateContent c) conn
          void $ runPQ (User.linkVerifiedContent canonicalEmail) conn
          (results, _) <- runPQ (getExpiredActive (30 :: Minute)) conn
          results `shouldBe` []
