{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.ContentURISpec
  ( main
  , spec
  ) where

import qualified Data.Text                         as T
import           Servant                           (fromText)
import           Test.Hspec                        (Spec, describe,
                                                    hspec, it, shouldBe)

import           ZoomHub.Types.Internal.ContentURI (ContentURI, unContentURI)


-- International Resource Locator
iri :: T.Text
iri = "http://doyoucity.com/site_media/entradas/panels/plan_vélez_2.jpg"

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "fromText" $
    it "supports IRI (Internationalized Resource Identifier)" $
      unContentURI <$> (fromText iri :: Maybe ContentURI) `shouldBe` Just iri
