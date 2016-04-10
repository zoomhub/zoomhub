{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Log.Logger
  ( logDebug
  , logDebugT
  , logDebug_
  , logError
  , logError_
  , logException
  , logException_
  , logInfo
  , logInfoT
  , logInfo_
  , logWarning
  , logWarning_
  ) where

import           Prelude                      hiding (log)

import           Control.Exception            (SomeException)
import           Data.Aeson                   (encode, object, (.=))
import           Data.Aeson.Types             (Pair)
import qualified Data.ByteString.Lazy         as BL
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Data.Text.Encoding           (decodeUtf8)
import qualified Data.Text.IO                 as TIO
import           Data.Time.Clock              (getCurrentTime)
import           Data.Time.Units              (Millisecond)
import           System.IO                    (stderr, stdout)
import           System.TimeIt                (timeItT)

import           ZoomHub.Types.Time.Instances ()

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

logDebugT :: String -> [Pair] -> IO a -> IO a
logDebugT = logT Debug

logInfo :: String -> [Pair] -> IO ()
logInfo = log Info

logInfoT :: String -> [Pair] -> IO a -> IO a
logInfoT = logT Info

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

logT :: Level -> String -> [Pair] -> IO a -> IO a
logT level msg meta action = do
  (duration, result) <- timeItT action
  log level msg
    (meta ++ [ "duration" .= (round (duration * 1000) :: Millisecond) ])
  return result

log :: Level -> String -> [Pair] -> IO ()
log level message meta = do
    now <- getCurrentTime
    TIO.hPutStrLn (handle level) $ decodeUtf8 . BL.toStrict $ (line now) <> "\n"
  where
    line time = encode . object $
      [ "time" .= time
      , "type" .= ("app" :: Text)
      , "level" .= show level
      , "message" .= message
      ] ++ meta

    handle Error = stderr
    handle _     = stdout
