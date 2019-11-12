{-# LANGUAGE OverloadedStrings #-}

module MigrateDatabase (main) where

import qualified ZoomHub.Storage.PostgreSQL.ConnectInfo as ConnectInfo
import ZoomHub.Storage.PostgreSQL.Schema (migrations)

import qualified Data.ByteString.Char8 as BC
import Squeal.PostgreSQL.Migration (defaultMain)
import System.Environment (getArgs, withArgs)
import System.Exit (die)


main :: IO ()
main = do
  args <- getArgs
  case args of
    (database:rest) -> do
      connectInfo <- ConnectInfo.fromEnv database
      let program = defaultMain
                      (ConnectInfo.connectionString connectInfo) migrations
      withArgs rest program
    _ ->
      die "Missing argument DATABASE"
