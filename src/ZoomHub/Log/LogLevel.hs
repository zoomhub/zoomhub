module ZoomHub.Log.LogLevel
  ( LogLevel (..),
    parse,
  )
where

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord)

instance Show LogLevel where
  show Debug = "debug"
  show Info = "info"
  show Warning = "warning"
  show Error = "error"

parse :: String -> Maybe LogLevel
parse "debug" = Just Debug
parse "info" = Just Info
parse "warning" = Just Warning
parse "error" = Just Error
parse _ = Nothing
