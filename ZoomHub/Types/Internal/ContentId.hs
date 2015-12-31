{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.ContentId
  ( ContentId
  , fromInteger
  , fromLBS
  ) where


import Prelude hiding (fromInteger)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as AC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified GHC.Generics as GHC
import qualified Servant as S


newtype ContentId = ContentId String
  deriving (Eq, GHC.Generic)

instance Show ContentId where
  show (ContentId cId) = cId

instance S.FromText ContentId where
  fromText t = Just $ ContentId $ T.unpack t

instance Aeson.ToJSON ContentId where
   toJSON = Aeson.genericToJSON $ AC.aesonPrefix AC.camelCase
instance Aeson.FromJSON ContentId where
   parseJSON = Aeson.genericParseJSON $ AC.aesonPrefix AC.camelCase

fromInteger :: (Integer -> String) -> Integer -> ContentId
fromInteger encode intId = ContentId $ encode intId

fromLBS :: LBS.ByteString -> ContentId
fromLBS cId = ContentId $ CL.unpack cId
