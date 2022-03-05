{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page.ExploreRecentContent
  ( ExploreRecentContent (..),
  )
where

import Data.Foldable (forM_, for_)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Lucid as H
import qualified ZoomHub.API.Types.Content as Content
import ZoomHub.API.Types.DeepZoomImage as DZI
import ZoomHub.Types.BaseURI (BaseURI)
import qualified ZoomHub.Types.Content as Content
import qualified ZoomHub.Types.Content as Internal
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import ZoomHub.Types.ContentId (unContentId)
import ZoomHub.Utils (tshow)
import ZoomHub.Web.Page (Page (Page), Title (..))
import qualified ZoomHub.Web.Page as Page

data ExploreRecentContent = ExploreRecentContent
  { ercContent :: [Internal.Content],
    ercBaseURI :: BaseURI,
    ercContentBaseURI :: ContentBaseURI
  }

instance H.ToHtml ExploreRecentContent where
  toHtml ExploreRecentContent {..} =
    Page.layout
      ( Page
          { pageTitle = Title $ "Explore: Recent — " <> Page.title,
            pageCanonicalPath = Nothing,
            pageBody =
              H.div_ [H.class_ "flex flex-col justify-center max-w-max mx-auto space-y-4 py-6"] do
                H.h1_ [H.class_ "text-3xl text-white font-bold"] "Explore: Recent"
                forM_ ercContent \internalContent ->
                  H.div_ [] do
                    let content = Content.fromInternal ercBaseURI ercContentBaseURI internalContent
                    for_ (Content.contentDzi content) \dzi ->
                      H.a_ [H.href_ $ "/" <> (T.pack . unContentId . Internal.contentId $ internalContent)] $
                        H.img_
                          [ H.src_ $ tshow $ DZI.largestSingleTileUrl dzi,
                            H.width_ "256",
                            H.style_ ("aspect-ratio: " <> tshow (dziWidth dzi) <> " / " <> tshow (dziHeight dzi))
                          ]
                    H.span_ [H.class_ "text-gray-400 text-xs"] $
                      H.toHtml $ tshow $ Content.contentInitializedAt internalContent
          }
      )

  toHtmlRaw = H.toHtml
