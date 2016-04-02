#!/usr/bin/env stack runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import           Data.Char            (ord)
import           Data.Csv
import qualified Data.Vector          as V
import System.Environment (getArgs)

data Content = Content
  { partitionKey    :: !String
  , rowKey          :: !String
  , timestamp       :: !String
  , abuseLevel      :: !Int
  , attributionLink :: !String
  , attributionText :: !String
  , blob            :: !String
  , error_          :: !String
  , id_             :: !String
  , mime            :: !String
  , numAbuseReports :: !Int
  , progress        :: !Int
  , size            :: !Int
  , stage           :: !Int
  , title           :: !String
  , type_           :: !String
  , url             :: !String
  , version         :: !Int
  } deriving Eq

decodeOptions = defaultDecodeOptions {
  decDelimiter = fromIntegral (ord '\t')
}

instance FromNamedRecord Content where
    parseNamedRecord r = Content <$> r .: "PartitionKey" <*> r .: "RowKey" <*>
      r .: "Timestamp" <*> r .: "AbuseLevel" <*> r .: "AttributionLink" <*>
      r .: "AttributionText" <*> r .: "Blob" <*> r .: "Error" <*>
      r .: "Id" <*> r .: "Mime" <*> r .: "NumAbuseReports" <*>
      r .: "Progress" <*> r .: "Size" <*> r .: "Stage" <*> r .: "Title" <*>
      r .: "Type" <*> r .: "Url" <*> r .: "Version"

main :: IO ()
main = do
    args <- getArgs
    case args of
      (file:_) -> do
        csvData <- BL.readFile file
        case decodeByNameWith decodeOptions csvData of
            Left err -> putStrLn err
            Right (_, v) -> V.forM_ v $ \p ->
                putStrLn $ "ID: " ++ id_ p ++ ", URL: " ++ url p
      _ -> error "Please provide a filename."
