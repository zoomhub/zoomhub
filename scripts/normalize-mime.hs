{-# LANGUAGE OverloadedStrings #-}

import           Codec.MIME.Parse       (parseMIMEType)
import           Codec.MIME.Type        (showType)
import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, execute, open, query_,
                                         withTransaction)

main :: IO ()
main = do
    conn <- open "./data/content-development.sqlite3"
    withTransaction conn $ do
      results <-
        query_ conn "SELECT id, mime FROM content" :: IO [(Integer, Maybe Text)]
      putStrLn $ "Found records: " ++ show (length results)
      mapM_ (normalizeMIMEType conn) results
      putStrLn "Done"
  where
    normalizeMIMEType :: Connection -> (Integer, Maybe Text) -> IO ()
    normalizeMIMEType conn (id_, maybeMIME) =
      case maybeMIME of
        Just mime ->
          execute conn "UPDATE content SET mime = ? WHERE id = ?"
            (showType <$> parseMIMEType mime, id_)
        Nothing -> return ()
