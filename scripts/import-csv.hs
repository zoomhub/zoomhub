#!/usr/bin/env stack runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Monad        (forM_)
import           Data.Either          (isLeft)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import System.Environment (getArgs)

data Content = Content
  { partitionKey    :: !Text -- :: !String
  , rowKey          :: !Text -- :: !String
  , timestamp       :: !Text -- :: !String
  , abuseLevel      :: !Text -- :: !Int
  , attributionLink :: !Text -- :: !String
  , attributionText :: !Text -- :: !String
  , blob            :: !Text -- :: !String
  , error_          :: !Text -- :: !String
  , id_             :: !Text -- :: !String
  , mime            :: !Text -- :: !String
  , numAbuseReports :: !Text -- :: !Int
  , progress        :: !Text -- :: !Int
  , size            :: !Text -- :: !Int
  , stage           :: !Text -- :: !Int
  , title           :: !Text -- :: !String
  , type_           :: !Text -- :: !String
  , url             :: !Text -- :: !String
  , version         :: !Text -- :: !Int
  } deriving Eq

instance Show Content where
  show Content {..} =
    "Content { partitionKey = " ++ show partitionKey ++ "\n" ++
    ", rowKey = " ++ show rowKey ++ "\n" ++
    ", timestamp = " ++ show timestamp ++ "\n" ++
    ", abuseLevel = " ++ show abuseLevel ++ "\n" ++
    ", attributionLink = " ++ show attributionLink ++ "\n" ++
    ", attributionText = " ++ show attributionText ++ "\n" ++
    ", blob = " ++ show blob ++ "\n" ++
    ", version = " ++ show version ++ "\n" ++
    ", error_ = " ++ show error_ ++ "\n" ++
    ", id_ = " ++ show id_ ++ "\n" ++
    ", mime = " ++ show mime ++ "\n" ++
    ", numAbuseReports = " ++ show numAbuseReports ++ "\n" ++
    ", progress = " ++ show progress ++ "\n" ++
    ", size = " ++ show size ++ "\n" ++
    ", stage = " ++ show stage ++ "\n" ++
    ", title = " ++ show title ++ "\n" ++
    ", type_ = " ++ show type_ ++ "\n" ++
    ", url = " ++ show url ++ "\n" ++
    "}"

toContent :: [Text] -> Either Text Content
toContent [] = Left "nothing"
-- toContent (partitionKey:rowKey:timestamp:abuseLevel:blob:error_:id_:mime:numAbuseReports:progress:size:stage:type_:url:version:attributionLink:attributionText:title:rest) -- v3
toContent (partitionKey:rowKey:timestamp:abuseLevel:attributionLink:attributionText:blob:error_:id_:mime:numAbuseReports:progress:size:stage:title:type_:url:version:rest) -- v4
  | null rest = Right Content {..}
  | otherwise = Left $ "more columns" <> T.pack (show rest)
toContent bad = Left $ "bad text: " <> T.pack (show bad)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file:_) -> do
      csvData <- T.readFile file
      let rawContent = map (T.split (=='\t')) (T.lines csvData)
      forM_ (map toContent rawContent) print
    _ -> error "Please provide a filename."
