{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Types.Embed
  ( Embed
  , EmbedDimension(..)
  , embedBody
  , embedContentId
  , embedDZI
  , embedHeight
  , embedWidth
  , mkEmbed
  ) where

import           Data.List                            (intercalate)
import           Data.Maybe                           (fromMaybe)
import           GHC.Generics                         (Generic)

import           ZoomHub.API.ContentTypes             (ToJS, toJS)
import           ZoomHub.Types.Internal.ContentId     (ContentId, unId)
import           ZoomHub.Types.Internal.DeepZoomImage


-- Constants
cssClassName :: String
-- Preserve `__seadragon` for backwards compatibility:
cssClassName = "__zoomhub __seadragon"

style :: Maybe EmbedDimension -> Maybe EmbedDimension -> String
style maybeWidth maybeHeight = unwords
  [ "background: black;"
  , "border: 1px solid black;"
  , "color: white;"
  , "height: " ++ toCSS height ++ ";"
  , "margin: 0;"
  , "padding: 0;"
  , "width: " ++ toCSS width ++ ";"
  ]
  where
    height = fromMaybe (Pixels 200) maybeHeight
    width = fromMaybe Auto maybeWidth

-- Types
data EmbedDimension = Auto | Pixels Integer
  deriving (Eq, Generic, Show)

toCSS :: EmbedDimension -> String
toCSS Auto = "auto"
toCSS (Pixels n) = show n ++ "px"

data Embed = Embed
  { embedBody      :: String
  , embedContentId :: ContentId
  , embedDZI       :: DeepZoomImage
  , embedHeight    :: Maybe EmbedDimension
  , embedId        :: String
  , embedWidth     :: Maybe EmbedDimension
  } deriving (Eq, Generic, Show)

mkEmbed :: String ->
           ContentId ->
           String ->
           DeepZoomImage ->
           Maybe EmbedDimension ->
           Maybe EmbedDimension ->
           Embed
mkEmbed embedId embedContentId embedBody embedDZI embedWidth embedHeight =
  Embed{..}

-- HTML
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
        [ ("class", cssClassName)
        , ("id", embedId embed)
        , ("style", style (embedWidth embed) (embedHeight embed))
        ]
      wrapper = concatPretty
        [ "(function () {"
        , "    document.write('" ++ html ++ "'});"
        , "    OpenSeadragon({});"
        , "}());"
        ]
      cId = unId . embedContentId $ embed
      script = embedBody embed
