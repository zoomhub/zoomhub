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
import ZoomHub.Types.Content (Content (contentURL))
import ZoomHub.Types.ContentURI (ContentURI(unContentURI))
import Control.Monad (forM_)

data Dashboard = Dashboard
  { session :: Session
  , content :: [Content]
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
                H.div_ [H.class_ "space-y-10 text-white"] do
                  H.p_ [] $ H.toHtml
                    case currentUser |> User.givenName of
                      Just givenName -> "Hi " <> givenName
                      Nothing -> "Hello"
                  H.ol_ [] $
                    forM_ content \c ->
                      H.li_ [] (H.toHtml (c |> contentURL |> unContentURI))
          }
      )
    where
      currentUser = session |> Session.currentUser

  toHtmlRaw = H.toHtml
