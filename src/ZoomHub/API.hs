{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ZoomHub.API
  ( app
  ) where

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Either           (EitherT, left)
import qualified Data.ByteString.Char8                as BC
import qualified Data.ByteString.Lazy.Char8           as BLC
import           Data.Monoid                          ((<>))
import           Data.Proxy                           (Proxy (Proxy))
import           Network.Wai                          (Application)
import           Servant                              ((:<|>) (..), (:>),
                                                       Capture, Get, JSON,
                                                       QueryParam, Raw,
                                                       ServantErr, Server,
                                                       err301, err400, err404,
                                                       errBody, errHeaders,
                                                       serve, serveDirectory)
import           Servant.HTML.Lucid                   (HTML)
import           System.Random                        (randomIO)

import           ZoomHub.API.ContentTypes             (JavaScript)
import           ZoomHub.Config                       (Config)
import qualified ZoomHub.Config                       as Config
import           ZoomHub.Storage.File                 (create, getById,
                                                       getByURL)
import           ZoomHub.Types.Content                (Content, fromInternal)
import           ZoomHub.Types.Embed                  (Embed, mkEmbed)
import           ZoomHub.Types.EmbedParam             (EmbedParam,
                                                       embedParamContentId,
                                                       embedParamHeight,
                                                       embedParamWidth)
import qualified ZoomHub.Types.Internal.Content       as Internal
import           ZoomHub.Types.Internal.ContentId     (ContentId, unId)
import           ZoomHub.Types.Internal.DeepZoomImage (dziHeight, dziTileFormat,
                                                       dziTileOverlap,
                                                       dziTileSize, dziWidth)
import qualified ZoomHub.Types.Internal.DeepZoomImage as Internal


-- Servant default handler type
type Handler a = EitherT ServantErr IO a

-- API
type API =
  -- TODO: Figure out how to route to `/`. Apparently `""` nor `"/"` works
  -- despite a hint here: https://git.io/vzEZx
       "health" :> Get '[HTML] String
  :<|> "version" :> Get '[HTML] String
  :<|> "v1" :> "content" :> Capture "id" ContentId :> Get '[JSON] Content
  :<|> "v1" :> "content" :> QueryParam "url" String :> Get '[JSON] Content
  :<|> Capture "embed" EmbedParam :> Get '[JavaScript] Embed
  :<|> Capture "viewId" ContentId :> Get '[HTML] Content
  :<|> Raw

-- API
api :: Proxy API
api = Proxy

server :: Config -> Server API
server config = health
           :<|> version (Config.version config)
           :<|> contentById (Config.dataPath config)
           :<|> contentByURL config
           :<|> embed (Config.openseadragonScript config)
           :<|> viewContentById (Config.dataPath config)
           :<|> serveDirectory (Config.publicPath config)

app :: Config -> Application
app config = serve api (server config)

-- Handlers
health :: Handler String
health = return "up"

version :: String -> Handler String
version = return

contentById :: FilePath -> ContentId -> Handler Content
contentById dataPath contentId = do
  maybeContent <- liftIO $ getById dataPath contentId
  case maybeContent of
    Nothing      -> left err404{ errBody = error404message }
    Just content -> return $ fromInternal content
  where
    error404message = "No content with ID: " <> rawContentId
    rawContentId = BLC.pack $ unId contentId

contentByURL :: Config -> Maybe String -> Handler Content
contentByURL config maybeURL = case maybeURL of
  Nothing  -> left err400{ errBody = "Missing ID or URL." }
  Just url -> do
      maybeContent <- liftIO $ getByURL (Config.dataPath config) url
      content <- case maybeContent of
        Nothing -> liftIO $ create config url
        Just c  -> return c
      redirect $ Internal.contentId content
      where
        -- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
        -- permanent HTTP 301 redirects:
        redirect contentId =
          let location = BC.pack $ "/v1/content/" ++ unId contentId in
          left $ err301{
            -- HACK: Redirect using error: http://git.io/vBCz9
            errHeaders = [("Location", location)]
          }

embed :: String -> EmbedParam -> Handler Embed
embed script param = do
  -- TODO: Why do we even enforce having an element ID for embed?
  randomId <- liftIO (randomIO :: IO Int)
  let eId = namespace ++ "-" ++ show (abs randomId)
  return $ mkEmbed eId cId script dzi width height
  where
    namespace = "__zoomhub"
    cId = embedParamContentId param
    width = embedParamWidth param
    height = embedParamHeight param
    dzi = Internal.DeepZoomImage
      { dziWidth = 1000
      , dziHeight = 1000
      , dziTileSize = 254
      , dziTileOverlap = 1
      , dziTileFormat = "jpg"
      }

viewContentById :: FilePath -> ContentId -> Handler Content
viewContentById dataPath contentId = do
  maybeContent <- liftIO $ getById dataPath contentId
  case maybeContent of
    Nothing      -> left err404{ errBody = error404message }
    Just content -> return $ fromInternal content
  where error404message = "No content with ID: " <> BLC.pack (unId contentId)
