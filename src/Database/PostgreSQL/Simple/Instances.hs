{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.PostgreSQL.Simple.Instances where

import           Data.Aeson                 (ToJSON, object, toJSON, (.=))
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (ConnectInfo (..))

instance ToJSON ConnectInfo where
  toJSON ConnectInfo{..} = object
    [ "host" .= connectHost
    , "port" .= connectPort
    , "user" .= connectUser
    , "password" .= ("<redacted>" :: Text)
    , "database" .= connectDatabase
    ]
