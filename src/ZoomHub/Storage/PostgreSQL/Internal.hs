{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ZoomHub.Storage.PostgreSQL.Internal where

import           Data.Maybe                      (fromMaybe)
import           Data.Profunctor.Product.Default (Default)
import           Opaleye                         (Query, Unpackspec, showSql)

printSQL :: Default Unpackspec a a => Query a -> IO ()
printSQL q = putStrLn $ fromMaybe "" (showSql q)
