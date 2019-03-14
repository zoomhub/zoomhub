{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ZoomHub.Storage.PostgreSQL.Internal where


import Data.Maybe (fromMaybe)
import Data.Pool (Pool, createPool)
import Data.Profunctor.Product.Default (Default)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.Units (Second, TimeUnit, toMicroseconds)
import qualified Database.PostgreSQL.Simple as PGS
import Opaleye (Query, Unpackspec, showSql)


printSQL :: Default Unpackspec a a => Query a -> IO ()
printSQL q = putStrLn $ fromMaybe "" (showSql q)

toNominalDiffTime :: (TimeUnit a) => a -> NominalDiffTime
toNominalDiffTime duration = fromIntegral $
  toMicroseconds duration `div`
  toMicroseconds (1 :: Second)

subtractUTCTime :: UTCTime -> NominalDiffTime -> UTCTime
subtractUTCTime time amount = addUTCTime (-amount) time

createConnectionPool :: (TimeUnit a) =>
                        PGS.ConnectInfo ->
                        Integer ->
                        a ->
                        Integer ->
                        IO (Pool PGS.Connection)
createConnectionPool connInfo numStripes idleTime maxResourcesPerStripe =
  createPool (PGS.connect connInfo) PGS.close (fromIntegral numStripes)
    (toNominalDiffTime idleTime) (fromIntegral maxResourcesPerStripe)
