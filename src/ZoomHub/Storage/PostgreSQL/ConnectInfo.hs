module ZoomHub.Storage.PostgreSQL.ConnectInfo
  ( ConnectInfo (..),
    fromEnv,
    connectionString,
    defaultConnectInfo,
  )
where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple
  ( ConnectInfo (..),
    defaultConnectInfo,
    postgreSQLConnectionString,
  )
import System.Environment (getEnvironment)
import Text.Read (readMaybe)

fromEnv :: String -> IO ConnectInfo
fromEnv dbName = do
  env <- getEnvironment
  let defaultDBHost = connectHost defaultConnectInfo
      defaultDBPort = connectPort defaultConnectInfo
      defaultDBUser = connectUser defaultConnectInfo
      defaultDBPassword = connectPassword defaultConnectInfo
  return $ ConnectInfo
    { connectHost = fromMaybe defaultDBHost (lookup "PGHOST" env),
      connectPort = fromMaybe defaultDBPort (lookup "PGPORT" env >>= readMaybe),
      connectUser = fromMaybe defaultDBUser (lookup "PGUSER" env),
      connectPassword = fromMaybe defaultDBPassword (lookup "PGPASSWORD" env),
      connectDatabase = fromMaybe dbName (lookup "PGDATABASE" env)
    }

connectionString :: ConnectInfo -> ByteString
connectionString = postgreSQLConnectionString
