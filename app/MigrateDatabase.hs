{-# LANGUAGE OverloadedStrings #-}

module MigrateDatabase (main) where

import qualified ZoomHub.Storage.PostgreSQL.ConnectInfo as ConnectInfo
import ZoomHub.Storage.PostgreSQL.Schema (migrations)

import Squeal.PostgreSQL.Migration (defaultMain)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.Exit (die)


main :: IO ()
main = do
  args <- getArgs
  mHashidsSecret <- lookupEnv "HASHIDS_SALT"
  case (args, mHashidsSecret) of
    ((database:rest), Just hashidsSecret) -> do
      connectInfo <- ConnectInfo.fromEnv database
      let program = defaultMain (ConnectInfo.connectionString connectInfo)
                      (migrations hashidsSecret)
      withArgs rest program
    (_, Nothing) ->
      die "Missing environment variable HASHIDS_SALT"
    _ ->
      die "Missing argument DATABASE"
