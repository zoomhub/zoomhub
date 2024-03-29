{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page.Homepage
  ( Homepage (..),
  )
where

import Data.Text (Text)
import Flow
import Lucid (ToHtml (toHtml))
import qualified Lucid as H
import NeatInterpolation (text)
import qualified ZoomHub.Config.Kinde as Kinde
import ZoomHub.Utils (tshow)
import ZoomHub.Web.Page (Page (Page), Title (..))
import qualified ZoomHub.Web.Page as Page

data Homepage = Homepage
  { kindeConfig :: Kinde.Config,
    kindeOAuthState :: Text
  }

instance H.ToHtml Homepage where
  toHtml homepage =
    Page.layout
      ( Page
          { Page.pageTitle = Title Page.title,
            Page.pageCanonicalPath = Just $ Page.Path "/",
            Page.pageHeadStyles = Just do
              H.style_
                [text|
                  /* Prevent blue outline: https://github.com/openseadragon/openseadragon/issues/893*/
                  .openseadragon-canvas:focus {
                    outline: none;
                  }
                |],
            Page.pageBodyClassName = Just "bg-gray-900 relative",
            Page.pageBody = do
              H.a_ [H.id_ "create"] mempty
              H.header_
                [H.class_ "bg-gray-900 border-b-2 md:border-b-0 md:border-t-2 border-orange-500 fixed z-10 w-full"]
                do
                  H.div_ [H.class_ "container mx-auto px-4 flex justify-between items-center"] do
                    H.div_ [H.class_ "inline-flex items-center h-10 md:h-16"] do
                      H.a_ [H.href_ "/"] mempty
                      H.div_ [H.class_ "bg-orange-500 mr-1 rounded-full w-4 h-4 md:w-7 md:h-7"] mempty
                      H.h1_
                        [H.class_ "text-xl md:text-4xl tracking-tighter text-gray-100 font-bold select-none inline-block"]
                        "ZoomHub"
                    H.nav_ [H.class_ "inline-flex"] $ do
                      H.a_
                        [ H.href_ "/showcase/ecommerce/watches",
                          H.class_ "nav-item border-0 border-r-2 border-gray-600 whitespace-nowrap hidden md:block"
                        ]
                        "Showcase: E-commerce"
                      H.a_
                        [ H.href_ "#create",
                          H.class_ "nav-item border-0 border-r-2 border-gray-600"
                        ]
                        "Create"
                      H.a_
                        [ H.href_ "#embed",
                          H.class_ "nav-item border-0 border-r-2 border-gray-600"
                        ]
                        "Embed"
                      H.a_
                        [ H.href_ "#api",
                          H.class_ "nav-item border-0 border-r-2 border-gray-600"
                        ]
                        "API"
                      let kindeDomain = homepage.kindeConfig.domain |> Kinde.unDomain
                          kindeClientId = homepage.kindeConfig.clientId |> Kinde.unClientId
                          kindeRedirectUri = homepage.kindeConfig.redirectURI |> tshow
                      H.a_
                        [ H.href_
                            ( mconcat
                                [ "https://" <> kindeDomain <> ".us.kinde.com/oauth2/auth?",
                                  "response_type=code",
                                  "&client_id=" <> kindeClientId,
                                  "&redirect_uri=" <> kindeRedirectUri,
                                  "&scope=openid%20profile%20email",
                                  "&state=" <> homepage.kindeOAuthState
                                ]
                            ),
                          H.class_ "nav-item pr-0"
                        ]
                        "Sign up"

              H.div_
                [H.class_ "flex flex-col justify-center max-w-max mx-auto py-6"]
                (homepage.kindeConfig.clientId |> Kinde.unClientId |> toHtml)
          }
      )

  toHtmlRaw = H.toHtml
