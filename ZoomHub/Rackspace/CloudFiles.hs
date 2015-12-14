{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Rackspace.CloudFiles where

import Control.Applicative as Applicative
import Control.Lens as Lens hiding ((.=)) -- .= used in Data.Aeson as well
import Data.Aeson as Aeson
import Data.Aeson.Lens as Aeson
import Network.Wreq as HTTP

import qualified Control.Exception as E
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Either as ET
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC


-- Types
data Credentials = Credentials
  { username :: String
  , apiKey :: String
  } deriving (Eq, Show)

instance Aeson.ToJSON Credentials where
    toJSON (Credentials username apiKey) =
      Aeson.object ["auth" .=
        Aeson.object ["RAX-KSKEY:apiKeyCredentials" .=
          Aeson.object ["username" .= username, "apiKey" .= apiKey]
        ]
      ]

newtype Endpoint = Endpoint String deriving Eq
instance Show Endpoint where
  show (Endpoint e) = e

newtype Token = Token String deriving Eq
instance Show Token where
  show (Token t) = t

-- API
tokenURL :: String
tokenURL = "https://identity.api.rackspacecloud.com/v2.0/tokens"

getMetadata :: Credentials -> IO LBS.ByteString
getMetadata credentials = do
  res <- HTTP.post tokenURL $ Aeson.toJSON credentials
  return $ res ^. responseBody

parseToken :: LBS.ByteString -> Maybe Token
parseToken meta =
  let maybeToken = meta ^? key "access" . key "token" . key "id" . _String in
  (Token . T.unpack) <$> maybeToken

parseEndpoint :: LBS.ByteString -> Maybe Endpoint
parseEndpoint meta =
  -- TODO: How do I filter `access.serviceCatalog[].name == "IAD"` using
  -- lenses from:
  -- `{"access":{"serviceCatalog":[{"name":"IAD","endpoints":[]}]}}`
  -- let a = meta ^? key "access" . key "serviceCatalog" . _Array in
  Just $ Endpoint "https://storage101.iad3.clouddrive.com/v1/MossoCloudFS_0c5dc6c2-028f-4648-a59d-e770b827add7"

getContent :: Credentials -> String -> IO (Maybe LBS.ByteString)
getContent credentials urlPath = do
  meta <- getMetadata credentials
  case parseToken meta of
    Nothing -> return Nothing
    Just t ->
      let opts = defaults & header "X-Auth-Token" .~ [B.pack $ show t] in
      case parseEndpoint meta of
        Nothing -> return Nothing
        Just e  -> do
          eitherRes <-
            E.tryJust (M.guard . is404) (HTTP.getWith opts (show e ++ urlPath))
          case eitherRes of
            ET.Right res -> return $ Just $ res ^. responseBody
            _            -> return Nothing
    where
      is404 :: HC.HttpException -> Bool
      is404 (HC.StatusCodeException s _ _)
        | s ^. statusCode == 404 = True
        | otherwise = False
