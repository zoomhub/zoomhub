{-# LANGUAGE OverloadedStrings #-}

module MigrateDatabase
  ( main,
  )
where

import Squeal.PostgreSQL.Session.Migration (mainMigrateIso)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.Exit (die)
import qualified ZoomHub.Storage.PostgreSQL.ConnectInfo as ConnectInfo
import qualified ZoomHub.Storage.PostgreSQL.Schema as Schema

main :: IO ()
main = do
  args <- getArgs
  mHashidsSecret <- lookupEnv "HASHIDS_SALT"
  case (args, mHashidsSecret) of
    (database : rest, Just hashidsSecret) -> do
      connectInfo <- ConnectInfo.fromEnv database
      let program =
            mainMigrateIso
              (ConnectInfo.connectionString connectInfo)
              (Schema.migrations hashidsSecret)
      withArgs rest program
    (_, Nothing) ->
      die "Missing environment variable HASHIDS_SALT"
    _ ->
      die "Missing argument DATABASE"
