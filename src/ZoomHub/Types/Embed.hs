{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Types.Embed
  ( Embed
  , EmbedDimension(..)
  , embedBody
  , embedContent
  , embedHeight
  , embedWidth
  , mkEmbed
  ) where

import           Data.Aeson.Encode                       (encode)
import qualified Data.ByteString.Lazy.Char8              as BLC
import           Data.List                               (intercalate)
import           Data.Maybe                              (fromMaybe)
import           GHC.Generics                            (Generic)

import           ZoomHub.API.ContentTypes                (ToJS, toJS)
import           ZoomHub.Types.Content                   (Content, contentDzi)
import           ZoomHub.Types.DeepZoomImage             (mkDeepZoomImage)
import           ZoomHub.Types.EmbedDimension            (EmbedDimension (..),
                                                          toCSSValue)
import           ZoomHub.Types.OpenSeadragonTileSource   (fromDeepZoomImage)
import           ZoomHub.Types.OpenSeadragonViewerConfig (mkOpenSeadragonViewerConfig)


data Embed = Embed
  { embedBody    :: String
  , embedContent :: Content
  , embedHeight  :: Maybe EmbedDimension
  , embedId      :: String
  , embedWidth   :: Maybe EmbedDimension
  } deriving (Eq, Generic, Show)

mkEmbed :: String ->
           Content ->
           String ->
           Maybe EmbedDimension ->
           Maybe EmbedDimension ->
           Embed
mkEmbed embedId embedContent embedBody embedWidth embedHeight =
  Embed{..}

-- CSS
legacyCSSClassName :: String
legacyCSSClassName = "__seadragon"

cssClassNames :: [String]
cssClassNames = ["__zoomhub", legacyCSSClassName]

defaultHeight :: EmbedDimension
defaultHeight = Pixels 200

defaultWidth :: EmbedDimension
defaultWidth = Auto

style :: Maybe EmbedDimension -> Maybe EmbedDimension -> String
style maybeWidth maybeHeight = unwords
  [ "background: black;"
  , "border: 1px solid black;"
  , "color: white;"
  , "height: " ++ toCSSValue height ++ ";"
  , "margin: 0;"
  , "padding: 0;"
  , "width: " ++ toCSSValue width ++ ";"
  ]
  where
    height = fromMaybe defaultHeight maybeHeight
    width = fromMaybe defaultWidth maybeWidth

-- JavaScript
concatPretty :: [String] -> String
concatPretty = intercalate "\n"

concatScripts :: [String] -> String
concatScripts = intercalate ";\n"

tag :: String -> [(String, String)] -> String
tag name attrs =
  "<" ++ name ++ " " ++ unwords (map attr attrs) ++ "></" ++ name ++ ">"

attr :: (String, String) -> String
attr (name, value) = concat [name, "=", "\"", value, "\""]

instance ToJS Embed where
  toJS embed = concatScripts [script, wrapper]
    where
      html = tag "div"
        [ ("class", unwords cssClassNames)
        , ("id", embedId embed)
        , ("style", style (embedWidth embed) (embedHeight embed))
        ]
      wrapper = concatPretty
        [ "(function () {"
        , "  document.write('" ++ html ++ "');"
        , "  OpenSeadragon(" ++ BLC.unpack (encode viewerConfig) ++ ");"
        , "}());"
        ]
      script = embedBody embed
      maybeDZI = contentDzi . embedContent $ embed
      queuedDZI =
        mkDeepZoomImage "http://zoom.it/static/queued.dzi" 1592 652 254 1 "jpg"
      tileSource = fromDeepZoomImage $ fromMaybe queuedDZI maybeDZI
      viewerConfig = mkOpenSeadragonViewerConfig (embedId embed) tileSource
