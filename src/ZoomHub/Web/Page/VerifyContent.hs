{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page.VerifyContent
  ( VerifyContent,
    mkVerifyContent,
    VerificationResult (..),
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lucid
  ( ToHtml,
    a_,
    div_,
    h2_,
    href_,
    p_,
    style_,
    toHtml,
    toHtmlRaw,
  )
import ZoomHub.API.Types.Content (Content, contentShareUrl)
import ZoomHub.Types.BaseURI (BaseURI)
import qualified ZoomHub.Web.Page as Page

data VerifyContent = VerifyContent
  { vcResult :: VerificationResult,
    vcBaseURI :: BaseURI
  }
  deriving (Eq, Show)

data VerificationResult
  = Success Content
  | Error Text
  deriving (Eq, Show)

mkVerifyContent :: BaseURI -> VerificationResult -> VerifyContent
mkVerifyContent vcBaseURI vcResult = VerifyContent {..}

instance ToHtml VerifyContent where
  toHtml (VerifyContent {..}) =
    Page.template Page.title do
      div_
        [ style_ $
            T.intercalate
              ";"
              [ "display: flex",
                "flex-flow: column",
                "align-items: center",
                "justify-content: center",
                "height: 100%",
                "text-align: center"
              ]
        ]
        $ case vcResult of
          Success content -> do
            h2_
              [style_ "color: #fff;"]
              "Your upload is now being processed…"
            p_
              [style_ "color: #fff;"]
              "Hang in there, this may take up to a few minutes. We’ll redirect you as soon as it’s ready."
            a_
              [href_ (T.pack . show $ contentShareUrl content)]
              (toHtml . show $ contentShareUrl content)
          Error message -> do
            h2_
              [style_ "color: #fff;"]
              "Oops, something went wrong"
            p_
              [style_ "color: #fff;"]
              (toHtml message)

  toHtmlRaw = toHtml
