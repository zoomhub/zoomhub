{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Rackspace.CloudFiles
  ( Credentials
  , mkCredentials
  , getMetadata
  , Token
  , parseToken
  , Endpoint
  , parseEndpoint
  , getContent
  , putContent
  , Container
  , parseContainer
  , ObjectName
  , parseObjectName
  , unObjectName
  ) where

import qualified Codec.MIME.Type       as MIME
import           Control.Exception     (tryJust)
import           Control.Lens          as Lens hiding ((.=))
import           Control.Monad         (guard)
import           Data.Aeson            (ToJSON, Value (String), object, toJSON,
                                        (.=))
import           Data.Aeson.Lens       (key, _String)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import           Data.List             (intercalate, isPrefixOf)
import qualified Data.Text             as T
import           Network.HTTP.Client   (HttpException (..))
import           Network.Wreq          (Options, Status, defaults, getWith,
                                        header, post, putWith, responseBody,
                                        responseStatus, statusCode)
import           System.Envy           (Var, fromVar, toVar)

-- Types
data Credentials = Credentials
  { credUsername :: String
  , credAPIKey   :: String
  } deriving (Eq, Show)

mkCredentials :: String -> String -> Credentials
mkCredentials username apiKey =
  Credentials
    { credUsername = username
    , credAPIKey = apiKey
    }

instance ToJSON Credentials where
  toJSON (Credentials username apiKey) =
    object ["auth" .=
      object ["RAX-KSKEY:apiKeyCredentials" .=
        object ["username" .= username, "apiKey" .= apiKey]
      ]
    ]

newtype Endpoint = Endpoint { unEndpoint :: String } deriving (Eq, Show)
newtype Token = Token { unToken :: String } deriving (Eq, Show)
newtype Metadata = Metadata { unMetadata :: BL.ByteString } deriving (Eq, Show)

newtype Container = Container { unContainer :: String } deriving (Eq, Show)

instance Var Container where
  toVar = unContainer
  fromVar = parseContainer

parseContainer :: String -> Maybe Container
parseContainer s
  | '/' `notElem` s = Just (Container s)
  | otherwise = Nothing

newtype ObjectName = ObjectName { unObjectName :: String } deriving (Eq, Show)

parseObjectName :: String -> Maybe ObjectName
parseObjectName s
  | "/" `isPrefixOf` s = Nothing
  | otherwise = Just (ObjectName s)

instance ToJSON ObjectName where
  toJSON = String . T.pack . unObjectName

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
      let options = toOptions t in
      case parseEndpoint meta of
        Nothing -> return Nothing
        Just e  -> do
          eitherRes <-
            tryJust (guard . is404) (getWith options (unEndpoint e ++ urlPath))
          case eitherRes of
            Right res -> return $ Just $ res ^. responseBody
            _         -> return Nothing
    where
      is404 :: HttpException -> Bool
      is404 (StatusCodeException s _ _) = s ^. statusCode == 404
      is404 _ = False

putContent :: Metadata ->
              FilePath ->
              MIME.Type ->
              Container ->
              ObjectName ->
              IO (Maybe Status)
putContent meta path mime container objectName =
  case (parseToken meta, parseEndpoint meta) of
    (Just token, Just endpoint) -> do
      let options = addContentType mime . toOptions $ token
          url = intercalate "/" [unEndpoint endpoint,
            unContainer container, unObjectName objectName]
      body <- BL.readFile path
      res <- putWith options url body
      return . Just $ res ^. responseStatus
    _ -> return Nothing

-- Helper
toOptions :: Token -> Options
toOptions token = defaults & header "X-Auth-Token" .~ [BC.pack $ unToken token]

addContentType :: MIME.Type -> Options -> Options
addContentType mime options =
  options & header "Content-Type" .~ [BC.pack . T.unpack . MIME.showType $ mime]
