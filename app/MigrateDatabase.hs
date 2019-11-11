{-# LANGUAGE OverloadedStrings #-}

module MigrateDatabase (main) where

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
      let program = defaultMain ("host=localhost port=5432 dbname=" <> BC.pack database) migrations
      withArgs rest program
    _ ->
      die "Missing argument <database>"
