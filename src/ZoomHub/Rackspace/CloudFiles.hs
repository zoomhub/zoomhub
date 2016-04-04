{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Rackspace.CloudFiles
  ( Credentials
  , credUsername
  , credAPIKey
  , getMetadata
  , Token
  , parseToken
  , Endpoint
  , parseEndpoint
  , getContent
  , putContent
  ) where

import qualified Codec.MIME.Type       as MIME
import           Control.Exception     (tryJust)
import           Control.Lens          as Lens hiding ((.=))
import           Control.Monad         (guard)
import           Data.Aeson            (ToJSON, object, toJSON, (.=))
import           Data.Aeson.Lens       (key, _String)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text             as T
import           Network.HTTP.Client   (HttpException (..))
import           Network.Wreq          (defaults, getWith, header, post,
                                        responseBody, statusCode)

-- Types
data Credentials = Credentials
  { credUsername :: String
  , credAPIKey   :: String
  } deriving (Eq, Show)

instance ToJSON Credentials where
    toJSON (Credentials username apiKey) =
      object ["auth" .=
        object ["RAX-KSKEY:apiKeyCredentials" .=
          object ["username" .= username, "apiKey" .= apiKey]
        ]
      ]

newtype Endpoint = Endpoint { unEndpoint :: String } deriving (Eq, Show)
newtype Token = Token { unToken :: String } deriving (Eq, Show)
newtype Metadata = Metadata { unMetadata :: BL.ByteString } deriving Eq

-- API
tokenURL :: String
tokenURL = "https://identity.api.rackspacecloud.com/v2.0/tokens"

getMetadata :: Credentials -> IO Metadata
getMetadata credentials = do
  res <- post tokenURL $ toJSON credentials
  return $ Metadata (res ^. responseBody)

parseToken :: Metadata -> Maybe Token
parseToken meta =
  let tokenId = key "access" . key "token" . key "id" . _String
      maybeToken = unMetadata meta ^? tokenId in
  (Token . T.unpack) <$> maybeToken

parseEndpoint :: Metadata -> Maybe Endpoint
parseEndpoint _ =
  -- TODO: How do I filter `access.serviceCatalog[].name == "IAD"` using
  -- lenses from:
  -- `{"access":{"serviceCatalog":[{"name":"IAD","endpoints":[]}]}}`
  -- let a = (unMetadata meta) ^? key "access" . key "serviceCatalog" . _Array in
  Just $ Endpoint "https://storage101.iad3.clouddrive.com/v1/\
    \MossoCloudFS_0c5dc6c2-028f-4648-a59d-e770b827add7"

getContent :: Metadata -> String -> IO (Maybe BL.ByteString)
getContent meta urlPath =
  case parseToken meta of
    Nothing -> return Nothing
    Just t ->
      let opts = defaults & header "X-Auth-Token" .~ [BC.pack $ unToken t] in
      case parseEndpoint meta of
        Nothing -> return Nothing
        Just e  -> do
          eitherRes <-
            tryJust (guard . is404) (getWith opts (unEndpoint e ++ urlPath))
          case eitherRes of
            Right res -> return $ Just $ res ^. responseBody
            _         -> return Nothing
    where
      is404 :: HttpException -> Bool
      is404 (StatusCodeException s _ _) = s ^. statusCode == 404
      is404 _ = False

putContent :: Metadata -> FilePath -> MIME.Type -> IO (Maybe BL.ByteString)
putContent meta path mime = undefined
