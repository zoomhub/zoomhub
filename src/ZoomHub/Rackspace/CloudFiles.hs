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
  , unContainer
  , ObjectName
  , parseObjectName
  , unObjectName
  ) where


import qualified Codec.MIME.Type        as MIME
import           Control.Exception      (tryJust)
import           Control.Lens           (each, filtered, (&), (.~), (^.), (^..),
                                         (^?))
import           Control.Monad          (guard)
import           Control.Monad.Catch    (Handler)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Retry          (RetryPolicyM, RetryStatus, capDelay,
                                         fullJitterBackoff, limitRetries,
                                         logRetries, recovering)
import           Data.Aeson             (ToJSON, Value (String), object, toJSON,
                                         (.=))
import           Data.Aeson.Lens        (key, _Array, _Object, _String)
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import           Data.List              (intercalate, isPrefixOf)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time.Units        (Second, toMicroseconds)
import           Network.HTTP.Client    (HttpException (FailedConnectionException2, StatusCodeException))
import           Network.Wreq           (Options, Status, defaults, getWith,
                                         header, post, putWith, responseBody,
                                         responseStatus, statusCode)
import           System.Envy            (Var, fromVar, toVar)

import           ZoomHub.Log.Logger     (logWarning)

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

-- Container
newtype Container = Container { unContainer :: String } deriving (Eq, Show)

instance Var Container where
  toVar = unContainer
  fromVar = parseContainer

instance ToJSON Container where
  toJSON = String . T.pack . unContainer

parseContainer :: String -> Maybe Container
parseContainer s
  | '/' `notElem` s = Just (Container s)
  | otherwise = Nothing

-- ObjectName
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
parseEndpoint meta =
    let result = unMetadata meta ^.. key "access" . key "serviceCatalog" .
                  _Array . traverse . name "cloudFiles" . _Object . traverse .
                  _Array . traverse . region "IAD" . key "publicURL" .
                  _String . each in
    case result of
      "" -> Nothing
      endpoint -> Just (Endpoint endpoint)
  where
    name n = filtered $ \o -> o ^? key "name" . _String == Just n
    region r = filtered $ \o -> o ^? key "region" . _String == Just r

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

-- TODO: Add support for using a `wreq` session for improved performance:
putContent :: Metadata ->
              FilePath ->
              MIME.Type ->
              Container ->
              ObjectName ->
              IO (Maybe Status)
putContent meta path mime container objectName =
    recovering backoffPolicy handlers
      (\_ -> unsafePutContent meta path mime container objectName)
  where
    handlers = [httpErrorH]

    httpErrorH :: RetryStatus -> Handler IO Bool
    httpErrorH = logRetries testE logRetry

    testE :: (Monad m) => HttpException -> m Bool
    testE e = case e of
      FailedConnectionException2 _ _ _ _ -> return True
      _                                  -> return False

    logRetry :: Bool -> String -> IO ()
    logRetry shouldRetry errorMessage =
        logWarning "Retrying `CloudFiles.putContent` due to error"
          [ "error" .= errorMessage
          , "nextAction" .= next
          ]
      where
        next :: Text
        next = if shouldRetry then "retry" else "crash"

-- Helper
toOptions :: Token -> Options
toOptions token = defaults & header "X-Auth-Token" .~ [BC.pack $ unToken token]

addContentType :: MIME.Type -> Options -> Options
addContentType mime options =
  options & header "Content-Type" .~ [BC.pack . T.unpack . MIME.showType $ mime]

-- Retry
backoffPolicy :: (MonadIO m) => RetryPolicyM m
backoffPolicy =
    capDelay maxDelay $ fullJitterBackoff base <> limitRetries maxRetries
  where
    maxDelay = fromIntegral $ toMicroseconds (30 :: Second)
    base = fromIntegral $ toMicroseconds (1 :: Second)
    maxRetries = 10


unsafePutContent :: Metadata ->
                    FilePath ->
                    MIME.Type ->
                    Container ->
                    ObjectName ->
                    IO (Maybe Status)
unsafePutContent meta path mime container objectName =
  case (parseToken meta, parseEndpoint meta) of
    (Just token, Just endpoint) -> do
      let options = addContentType mime . toOptions $ token
          url = intercalate "/" [unEndpoint endpoint,
            unContainer container, unObjectName objectName]
      body <- BL.readFile path
      res <- putWith options url body
      return . Just $ res ^. responseStatus
    _ -> return Nothing
