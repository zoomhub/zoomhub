{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text                            as T
import           Servant                              (fromText)
import           Test.Hspec                           (context, describe, hspec,
                                                       it, shouldBe)
import           Test.QuickCheck                      (property)

import qualified ZoomHub.Storage.Internal.FileTest    as File
import qualified ZoomHub.Types.Internal.ContentIdTest as ContentId
import           ZoomHub.Types.Internal.ContentURI    (ContentURI)


-- International Resource Locator
iri :: T.Text
iri = "http://doyoucity.com/site_media/entradas/panels/plan_vélez_2.jpg"

main :: IO ()
main = hspec $ do
  describe "ZoomHub.Storage.File: `toId`" $
    context "when used with valid strings, i.e. no underscores," $
      it "is inverse to `toFilename`" $ property
        File.prop_invertible

  describe "ZoomHub.Types.Internal.ContentId: `encode`" $
    context "when used with valid strings, i.e. no underscores," $
      it "is inverse to `decode`" $ property
        ContentId.prop_invertible

  describe "ZoomHub.Types.Internal.ContentURI: `fromText`" $
    it "supports IRI (Internationalized Resource Identifier)" $
      show <$> (fromText iri :: Maybe ContentURI) `shouldBe` (show <$> Just iri)
