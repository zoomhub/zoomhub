{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Types.Embed
  ( Embed
  , embedBody
  , embedContent
  , embedHeight
  , embedWidth
  , mkEmbed
  ) where

import           Data.Aeson.Encode                       (encode)
import qualified Data.ByteString.Lazy.Char8              as BLC
import           Data.List                               (intercalate)
import           Data.Maybe                              (fromJust, fromMaybe)
import           GHC.Generics                            (Generic)
import           Network.URI                             (parseRelativeReference,
                                                          relativeTo)

import           ZoomHub.API.ContentTypes.JavaScript     (ToJS, toJS)
import           ZoomHub.Types.BaseURI                   (BaseURI)
import           ZoomHub.Types.Content                   (Content, contentDzi,
                                                          contentReady)
import           ZoomHub.Types.ContentBaseURI            (ContentBaseURI,
                                                          unContentBaseURI)
import           ZoomHub.Types.DeepZoomImage             (DeepZoomImageURI (..),
                                                          mkDeepZoomImage)
import           ZoomHub.Types.EmbedDimension            (EmbedDimension (..),
                                                          toCSSValue)
import           ZoomHub.Types.OpenSeadragonTileSource   (fromDeepZoomImage)
import           ZoomHub.Types.OpenSeadragonViewerConfig (mkOpenSeadragonViewerConfig)


data Embed = Embed
  { embedBaseURI        :: BaseURI
  , embedContentBaseURI :: ContentBaseURI
  , embedBody           :: String
  , embedContainerId    :: String
  , embedContent        :: Content
  , embedHeight         :: Maybe EmbedDimension
  , embedWidth          :: Maybe EmbedDimension
  } deriving (Eq, Generic, Show)

mkEmbed :: BaseURI ->
           ContentBaseURI ->
           String ->
           Content ->
           String ->
           Maybe EmbedDimension ->
           Maybe EmbedDimension ->
           Embed
mkEmbed baseURI contentBaseURI containerId content body width height = Embed{..}
  where
    embedBaseURI = baseURI
    embedContentBaseURI = contentBaseURI
    embedContainerId = containerId
    embedContent = content
    embedBody = body
    embedWidth = width
    embedHeight = height

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
        , ("id", embedContainerId embed)
        , ("style", style (embedWidth embed) (embedHeight embed))
        ]
      wrapper = concatPretty
        [ "(function () {"
        , "  document.write('" ++ html ++ "');"
        , "  OpenSeadragon(" ++ BLC.unpack (encode viewerConfig) ++ ");"
        , "}());"
        ]
      script = embedBody embed
      content = embedContent embed
      maybeDZI = contentDzi content
      queuedDZI = mkDeepZoomImage queuedDZIURI 8000 6000 254 1 "png"
      queuedDZIURI = DeepZoomImageURI $
        queuedDZIPath `relativeTo` unContentBaseURI contentBaseURI
      queuedDZIPath = fromJust . parseRelativeReference $
        "/static/queued.dzi"
      isReady = contentReady content
      tileSource
        | not isReady = fromDeepZoomImage queuedDZI
        | otherwise   = fromDeepZoomImage $ fromMaybe queuedDZI maybeDZI
      viewerConfig =
        mkOpenSeadragonViewerConfig contentBaseURI containerId tileSource
      contentBaseURI = embedContentBaseURI embed
      containerId = embedContainerId embed
