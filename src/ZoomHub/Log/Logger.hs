{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Log.Logger
  ( logDebug
  , logDebug_
  , logError
  , logError_
  , logException
  , logException_
  , logInfo
  , logInfo_
  , logWarning
  , logWarning_
  ) where

import           Prelude              hiding (log)

import           Control.Exception    (SomeException)
import           Data.Aeson           (encode, object, (.=))
import           Data.Aeson.Types     (Pair)
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8)
import qualified Data.Text.IO         as TIO
import           Data.Time.Clock      (getCurrentTime)
import           System.IO            (stderr, stdout)

data Level = Debug | Info | Warning | Error deriving Eq

instance Show Level where
  show Debug = "debug"
  show Info = "info"
  show Warning = "warning"
  show Error = "error"

logDebug :: String -> [Pair] -> IO ()
logDebug = log Debug

logDebug_ :: String -> IO ()
logDebug_ = log_ Debug

logInfo :: String -> [Pair] -> IO ()
logInfo = log Info

logInfo_ :: String -> IO ()
logInfo_ = log_ Info

logWarning :: String -> [Pair] -> IO ()
logWarning = log Warning

logWarning_ :: String -> IO ()
logWarning_ = log_ Warning

logError :: String -> [Pair] -> IO ()
logError = log Error

logError_ :: String -> IO ()
logError_ = log_ Error

logException :: String -> SomeException -> [Pair] -> IO ()
logException msg e meta = logError msg ( meta ++ [ "error" .= show e ])

logException_ :: String -> SomeException -> IO ()
logException_ msg e = logException msg e []

log_ :: Level -> String -> IO ()
log_ level message = log level message []

log :: Level -> String -> [Pair] -> IO ()
log level message meta = do
    now <- getCurrentTime
    TIO.hPutStrLn (handle level) $ decodeUtf8 . BL.toStrict $
      (line now) <> "\n"
  where
    line time = encode . object $
      [ "time" .= time
      , "type" .= ("app" :: Text)
      , "level" .= show level
      , "message" .= message
      ] ++ meta

    handle Error = stderr
    handle _     = stdout
