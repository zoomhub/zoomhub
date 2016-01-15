{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.ContentId
  ( ContentId
  , fromInteger
  , fromLBS
  , fromString
  , unId
  ) where


import Prelude hiding (fromInteger)
import Data.List (isInfixOf)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as AC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified GHC.Generics as GHC
import qualified Servant as S


-- TODO: Use record syntax, i.e. `ContentId { unId :: String}` without
-- introducing `{"id": <id>}` JSON serialization:
newtype ContentId = ContentId String
  deriving (Eq, GHC.Generic, Show)

instance S.FromText ContentId where
  fromText t = Just $ ContentId $ T.unpack t

instance Aeson.ToJSON ContentId where
   toJSON = Aeson.genericToJSON $ AC.aesonPrefix AC.camelCase
instance Aeson.FromJSON ContentId where
   parseJSON = Aeson.genericParseJSON $ AC.aesonPrefix AC.camelCase

unId :: ContentId -> String
unId (ContentId cId) = cId

fromInteger :: (Integer -> String) -> Integer -> ContentId
fromInteger encode intId = fromString $ encode intId

fromLBS :: LBS.ByteString -> ContentId
fromLBS contentId = fromString $ CL.unpack contentId

-- TODO: Change return type to `Maybe ContentId` to make it a total function:
fromString :: String -> ContentId
fromString cId
  | "_" `isInfixOf` cId = error "Content IDs cannot have underscores (`_`)"
  | otherwise           = ContentId cId
