{-# LANGUAGE OverloadedStrings #-}

module MigrateDatabase
  ( main,
  )
where

import Squeal.PostgreSQL.Migration (defaultMain)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.Exit (die)
import qualified ZoomHub.Storage.PostgreSQL.ConnectInfo as ConnectInfo
import ZoomHub.Storage.PostgreSQL.Schema (migrations)

main :: IO ()
main = do
  args <- getArgs
  mHashidsSecret <- lookupEnv "ZH_HASHIDS_SALT"
  case (args, mHashidsSecret) of
    ((database : rest), Just hashidsSecret) -> do
      connectInfo <- ConnectInfo.fromEnv database
      let program =
            defaultMain
              (ConnectInfo.connectionString connectInfo)
              (migrations hashidsSecret)
      withArgs rest program
    (_, Nothing) ->
      die "Missing environment variable ZH_HASHIDS_SALT"
    _ ->
      die "Missing argument DATABASE"
