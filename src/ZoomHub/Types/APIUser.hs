module ZoomHub.Types.APIUser
  ( APIUser (..),
  )
where

import Data.Text (Text)

data APIUser = APIUser
  { username :: Text,
    password :: Text
  }
