{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page.Dashboard
  ( Dashboard (..),
  )
where

import qualified Lucid as H
import Flow
import ZoomHub.Web.Page (Page (Page), Title (..))
import qualified ZoomHub.Web.Page as Page
import ZoomHub.Authentication.Session (Session)
import qualified ZoomHub.Authentication.Session as User
import qualified ZoomHub.Authentication.Session as Session
import qualified ZoomHub.Types.Content as Internal
import Control.Monad (forM_)
import Data.Foldable (for_)
import qualified ZoomHub.API.Types.Content as Content
import qualified Data.Text as T
import ZoomHub.Types.ContentId (ContentId(unContentId))
import qualified ZoomHub.API.Types.DeepZoomImage as DZI
import ZoomHub.Utils (tshow)
import ZoomHub.API.Types.DeepZoomImage (DeepZoomImage(dziWidth, dziHeight))
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)

data Dashboard = Dashboard
  { session :: Session
  , content :: [Internal.Content]
  , baseURI :: BaseURI
  , contentBaseURI :: ContentBaseURI
  }

instance H.ToHtml Dashboard where
  toHtml Dashboard {..} =
    Page.layout
      ( Page
          { pageTitle = Title $ "Dashboard — " <> Page.titleShort,
            pageCanonicalPath = Nothing,
            pageHeadStyles = Nothing,
            pageBodyClassName = Nothing,
            pageBody =
              H.div_ [H.class_ "p-6"] do
                H.h1_ [H.class_ "text-3xl text-white font-bold mb-2"] "Dashboard"
                H.div_ [H.class_ "text-white"] do
                  H.p_ [] $ H.toHtml
                    case currentUser |> User.givenName of
                      Just givenName -> "Hi " <> givenName
                      Nothing -> "Hello"
                  H.div_ [H.class_ "space-y-4"] $
                    forM_ content \internalContent ->
                      let publicContent = Content.fromInternal baseURI contentBaseURI internalContent in
                      for_ (publicContent |> Content.contentDzi) \dzi ->
                        H.a_ [H.href_ $ "/" <> (T.pack . unContentId . Internal.contentId $ internalContent)] $
                          H.img_
                            [ H.src_ $ tshow $ DZI.largestSingleTileUrl dzi,
                              H.width_ "256",
                              H.style_ ("aspect-ratio: " <> tshow (dziWidth dzi) <> " / " <> tshow (dziHeight dzi))
                            ]
                      -- H.li_ [] (H.toHtml (c |> contentURL |> unContentURI))
          }
      )
    where
      currentUser = session |> Session.currentUser

  toHtmlRaw = H.toHtml
