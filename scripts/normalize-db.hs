{-# LANGUAGE OverloadedStrings #-}

import           Codec.MIME.Parse       (parseMIMEType)
import           Codec.MIME.Type        (showType)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Database.SQLite.Simple (Connection, execute, open, query_,
                                         withTransaction)
import           System.Environment     (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (dbPath:_) -> do
      conn <- open dbPath
      withTransaction conn $ do
        results <- query_ conn "SELECT id, initializedAt, mime FROM content"
          :: IO [(Integer, UTCTime, Maybe Text)]
        putStrLn $ "Found " ++ show (length results) ++ " rows."
        mapM_ (normalize conn) results
        putStrLn "Done."
    _ -> error "Please provide the path to the SQLite3 database."
  where
    normalize :: Connection -> (Integer, UTCTime, Maybe Text) -> IO ()
    normalize conn (id_, initializedAt, maybeMIME) =
      case maybeMIME of
        Just mime -> do
          putStrLn $ "Updating row: " ++ show id_ ++
            " (initializedAt: " ++ show initializedAt ++ ")"
          execute conn "UPDATE content SET initializedAt = ?, mime = ?\
            \ WHERE id = ?"
            (initializedAt, showType <$> parseMIMEType mime, id_)
        Nothing -> do
          putStrLn $ "Invalid MIME: " ++ show id_
          return ()
