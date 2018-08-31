{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.ContentURISpec
  ( main
  , spec
  ) where

import           Data.Text                (Text)
import qualified Data.Text                as T
import           Servant                  (parseUrlPiece)
import           Test.Hspec               (Spec, describe, hspec, it, shouldBe)

import           ZoomHub.Types.ContentURI (ContentURI, unContentURI)


-- International Resource Locator
iri :: T.Text
iri = "http://doyoucity.com/site_media/entradas/panels/plan_vélez_2.jpg"

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "fromText" $
    it "supports IRI (Internationalized Resource Identifier)" $
      unContentURI <$> (parseUrlPiece iri :: Either Text ContentURI) `shouldBe` Right iri
