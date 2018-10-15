{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module ZoomHub.Types.ContentId
  ( ContentId
  , ContentId'
  , ContentIdColumn
  , fromInteger
  , fromString
  , isValid
  , mkContentId
  , pContentId
  , toColumn
  , unContentId
  , validChars
  ) where

import           Prelude                              hiding (fromInteger)

import           Data.Aeson                           (FromJSON, ToJSON,
                                                       genericParseJSON,
                                                       genericToJSON, parseJSON,
                                                       toJSON)
import           Data.Aeson.Casing                    (aesonPrefix, camelCase)
import           Data.List                            (intersperse)
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as PGS
import           Database.SQLite.Simple               (SQLData (SQLText))
import           Database.SQLite.Simple.FromField     (FromField, ResultError (ConversionFailed),
                                                       fromField, returnError)
import           Database.SQLite.Simple.Internal      (Field (Field))
import           Database.SQLite.Simple.Ok            (Ok (Ok))
import           Database.SQLite.Simple.ToField       (ToField, toField)
import           GHC.Generics                         (Generic)
import           Opaleye                              (Column, PGText,
                                                       QueryRunnerColumnDefault,
                                                       fieldQueryRunnerColumn,
                                                       pgString,
                                                       queryRunnerColumnDefault)
import           Servant                              (FromHttpApiData,
                                                       parseUrlPiece)
import           Squeal.PostgreSQL                    (FromValue (..),
                                                       PGType (PGtext), PG,
                                                       ToParam(..))

-- TODO: Use record syntax, i.e. `ContentId { unContentId :: String }` without
-- introducing `{"id": <id>}` JSON serialization:
newtype ContentId' a = ContentId a
  deriving (Eq, Functor, Generic, Show)
type ContentId = ContentId' String

unContentId :: ContentId -> String
unContentId (ContentId cId) = cId

mkContentId :: a -> ContentId' a
mkContentId = ContentId

fromInteger :: (Integer -> String) -> Integer -> ContentId
fromInteger encode intId = fromString $ encode intId

-- TODO: Change return type to `Maybe ContentId` to make it a total function:
fromString :: String -> ContentId
fromString s
  | isValid s = ContentId s
  | otherwise = error $ "Invalid content ID '" ++ s ++ "'." ++
    " Valid characters: " ++ intersperse ',' validChars ++ "."

-- NOTE: Duplicated from `hashids`: https://git.io/vgpT4
-- TODO: Use this for `hashids` initialization.
validChars :: String
validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

validCharsSet :: Set Char
validCharsSet = Set.fromList validChars

isValid :: String -> Bool
isValid = all (`Set.member` validCharsSet)

-- Text
instance FromHttpApiData ContentId where
  parseUrlPiece t
    | isValid s = Right . fromString $ s
    | otherwise = Left "Invalid content ID"
    where s = T.unpack t

-- JSON
instance ToJSON ContentId where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON ContentId where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

-- SQLite
instance ToField ContentId where
  toField = SQLText . T.pack . unContentId

instance FromField ContentId where
  fromField f@(Field (SQLText t) _) =
    case parseUrlPiece t of
      Right r  -> Ok r
      Left _ -> returnError ConversionFailed f "Invalid content ID"
  fromField f = returnError ConversionFailed f "Invalid content ID"

-- PostgreSQL
type ContentIdColumn = ContentId' (Column PGText)
$(makeAdaptorAndInstance "pContentId" ''ContentId')

toColumn :: ContentId -> ContentIdColumn
toColumn = fmap pgString

instance PGS.FromField ContentId where
  fromField f mdata = PGS.fromField f mdata >>= parseContentId
    where
      parseContentId t = case parseUrlPiece t of
        Right r -> return r
        Left _  -> PGS.returnError PGS.ConversionFailed f "Invalid content ID"

instance QueryRunnerColumnDefault PGText ContentId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- Squeal / PostgreSQL
type instance PG ContentId = 'PGtext
instance ToParam ContentId 'PGtext where
  toParam = toParam . unContentId

instance FromValue 'PGtext ContentId where
  fromValue = ContentId <$> fromValue @'PGtext
