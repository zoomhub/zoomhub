{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PG where

import           Prelude hiding (sum)

import Data.Int (Int64)
import           Opaleye (Column, Nullable, limit, matchNullable, isNull,
                         Table(Table), required, queryTable,
                         Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                         (.===),
                         (.++), ifThenElse, pgString, aggregate, groupBy,
                         count, avg, sum, leftJoin, runQuery,
                         showSqlForPostgres, Unpackspec,
                         PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool,
                         PGTimestamptz)

import           Data.Profunctor.Product (p7)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock    (UTCTime)
import           Data.Text       (Text)

import           Control.Arrow (returnA, (<<<))

import qualified Database.PostgreSQL.Simple as PGS

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres

imageTable :: Table (Column PGInt8, Column PGTimestamptz, Column PGInt8,
                     Column PGInt8, Column PGInt4, Column PGInt4, Column PGText)
                    (Column PGInt8, Column PGTimestamptz, Column PGInt8,
                     Column PGInt8, Column PGInt4, Column PGInt4, Column PGText)
imageTable = Table "image" (p7 ( required "contentid"
                               , required "initializedat"
                               , required "width"
                               , required "height"
                               , required "tilesize"
                               , required "tileoverlap"
                               , required "tileformat"
                               )
                           )

imageQuery :: Query (Column PGInt8, Column PGTimestamptz, Column PGInt8,
                     Column PGInt8, Column PGInt4, Column PGInt4, Column PGText)
imageQuery = queryTable imageTable

idWidthHeight :: Query (Column PGInt8, Column PGTimestamptz, Column PGInt8,
                        Column PGInt8, Column PGText)
idWidthHeight = proc () -> do
  (id_, createdAt, width, height, _, _, tileFormat) <- imageQuery -< ()
  returnA -< (id_, createdAt, width, height, tileFormat)


runImageQuery :: PGS.Connection ->
                 IO [(Int64, UTCTime, Int64, Int64, Text)]
runImageQuery conn = runQuery conn (limit 10 $ idWidthHeight)

dbConnectInfo :: PGS.ConnectInfo
dbConnectInfo = PGS.defaultConnectInfo { PGS.connectDatabase = "zoomhub-production" }

main :: IO ()
main = do
  conn <- PGS.connect dbConnectInfo
  res <- runImageQuery conn
  print $ res
