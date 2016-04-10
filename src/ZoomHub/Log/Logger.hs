{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Log.Logger
  ( logDebug
  , logDebug_
  , logInfo
  , logInfo_
  , logWarning
  , logWarning_
  , logError
  , logError_
  ) where

import           Prelude                    hiding (log)

import           Control.Concurrent         (myThreadId)
import           Data.Aeson                 (encode, object, (.=))
import           Data.Aeson.Types           (Pair)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Text                  (Text)
import           Data.Time.Clock            (getCurrentTime)

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

log_ :: Level -> String -> IO ()
log_ level message = log level message []

log :: Level -> String -> [Pair] -> IO ()
log level message meta = do
  now <- getCurrentTime
  threadId <- myThreadId
  putStrLn . BLC.unpack . encode . object $
    [ "time" .= now
    , "type" .= ("app" :: Text)
    , "level" .= show level
    , "message" .= message
    , "threadId" .= show threadId
    ] ++ meta
