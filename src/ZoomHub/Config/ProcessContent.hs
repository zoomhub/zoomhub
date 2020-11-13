module ZoomHub.Config.ProcessContent
  ( ProcessContent (..),
    parse,
  )
where

import Data.Aeson (ToJSON, Value (String), toJSON)
import qualified Data.Text as T

data ProcessContent
  = ProcessNoContent
  | ProcessExistingContent
  | ProcessExistingAndNewContent
  deriving (Eq, Show)

parse :: String -> ProcessContent
parse "ProcessExistingContent" = ProcessExistingContent
parse "ProcessExistingAndNewContent" = ProcessExistingAndNewContent
parse _ = ProcessNoContent

instance ToJSON ProcessContent where
  toJSON = String . T.pack . show
