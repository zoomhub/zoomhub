{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Types.Embed
  ( Embed
  , EmbedDimension(..)
  , mkEmbed
  , embedContentId
  , embedWidth
  , embedHeight
  , embedBody
  , embedDZI
  , tag
  ) where

import           Data.List                            (intersperse)
import           Data.Maybe                           (fromMaybe)
import           GHC.Generics                         (Generic)

import           ZoomHub.API.ContentTypes             (ToJS, toJS)
import           ZoomHub.Types.Internal.ContentId     (ContentId, unId)
import           ZoomHub.Types.Internal.DeepZoomImage


-- Constants
cssClassName :: String
cssClassName = "__seadragon"

style :: Maybe EmbedDimension -> Maybe EmbedDimension -> String
style maybeWidth maybeHeight = concat . intersperse " " $
  [ "background: black;"
  , "border: 1px solid black; "
  , "color: white;"
  , "height: " ++ toCSS height ++ ";"
  , "margin: 0;"
  , "padding: 0;"
  , "width: " ++ toCSS width ++ ";"
  ]
  where
    height = fromMaybe (Pixels 200) maybeHeight
    width = fromMaybe (Auto) maybeWidth

-- Types
data EmbedDimension = Auto | Pixels Integer
  deriving (Eq, Generic, Show)

toCSS :: EmbedDimension -> String
toCSS Auto = "auto"
toCSS (Pixels n) = show n ++ "px"

data Embed = Embed
  { embedId        :: Integer
  , embedContentId :: ContentId
  , embedBody      :: String
  , embedDZI       :: DeepZoomImage
  , embedWidth     :: Maybe EmbedDimension
  , embedHeight    :: Maybe EmbedDimension
  } deriving (Eq, Generic, Show)

mkEmbed :: Integer ->
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
concatPretty = concat . intersperse "\n"

concatScripts :: [String] -> String
concatScripts = concat . intersperse ";\n"

tag :: String -> [(String, String)] -> String
tag name attrs = "<" ++ name ++ " " ++ concatMap attr attrs ++ ">" ++
  "</" ++ name ++ ">"

attr :: (String, String) -> String
attr (name, value) = concat [name, "=\"", value, "\" "]

instance ToJS Embed where
  toJS embed = concatScripts $ [script, wrapper embed]
    where
      html = tag "div"
        [ ("class", cssClassName)
        , ("id", idAttr embed)
        , ("style", style (embedWidth embed) (embedHeight embed))
        ]
      wrapper embed = concatPretty
        [ "(function () {"
        , "    document.write('" ++ html ++ "'});"
        , "    OpenSeadragon({});"
        , "}());"
        ]
      cId = unId . embedContentId $ embed
      script = embedBody embed
      idAttr embed = cssClassName ++ show (embedId embed)
