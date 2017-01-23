{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module PG where

import           Data.Int                        (Int64)
import           Opaleye                         (Column, PGInt4, PGInt8,
                                                  PGText, PGTimestamptz, Query,
                                                  Table (Table), Unpackspec,
                                                  limit, queryTable, required,
                                                  runQuery, showSql)

import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Text                       (Text)
import           Data.Time.Clock                 (UTCTime)

import qualified Database.PostgreSQL.Simple      as PGS

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql q = case showSql q of
  Just t  -> putStrLn t
  Nothing -> return ()

data Image' a b c d e f g = Image
  { imageContentId     :: a
  , imageInitializedAt :: b
  , imageWidth         :: c
  , imageHeight        :: d
  , imageTileSize      :: e
  , imageTileOverlap   :: f
  , imageTileFormat    :: g
  } deriving (Show)
type Image = Image'
  Int64
  UTCTime
  Int64
  Int64
  Int
  Int
  Text
type ImageColumn = Image'
  (Column PGInt8)
  (Column PGTimestamptz)
  (Column PGInt8)
  (Column PGInt8)
  (Column PGInt4)
  (Column PGInt4)
  (Column PGText)

$(makeAdaptorAndInstance "pImage" ''Image')

imageTable :: Table ImageColumn ImageColumn
imageTable = Table "image"
                   (pImage Image
                     { imageContentId = required "contentid"
                     , imageInitializedAt = required "initializedat"
                     , imageWidth = required "width"
                     , imageHeight = required "height"
                     , imageTileSize = required "tilesize"
                     , imageTileOverlap = required "tileoverlap"
                     , imageTileFormat = required "tileformat"
                     }
                   )

imageQuery :: Query ImageColumn
imageQuery = queryTable imageTable

runImageQuery :: PGS.Connection -> Query ImageColumn -> IO [Image]
runImageQuery = runQuery

dbConnectInfo :: PGS.ConnectInfo
dbConnectInfo = PGS.defaultConnectInfo { PGS.connectDatabase = "zoomhub-production" }

main :: IO ()
main = do
  conn <- PGS.connect dbConnectInfo
  res <- runImageQuery conn (limit 10 imageQuery)
  print $ res
