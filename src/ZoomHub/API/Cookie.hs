{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.Cookie
  ( oauth2StateCookieHeader,
    oauth2StateCookieName,
    sessionCookieHeader,
    sessionCookieName,
  )
where

import qualified Web.ClientSession as ClientSession
import Web.Cookie (SetCookie)
import qualified ZoomHub.Authentication.Cookie as Cookie
import ZoomHub.Authentication.OAuth (AuthorizeState (..))
import ZoomHub.Authentication.Session (Session (..))

sessionCookieName :: Cookie.Name
sessionCookieName = Cookie.Name "__Host-zoomhub_session"

oauth2StateCookieName :: Cookie.Name
oauth2StateCookieName = Cookie.Name "__Host-zoomhub_oauth2_state"

sessionCookieHeader :: ClientSession.Key -> Session -> IO SetCookie
sessionCookieHeader key session =
  Cookie.setEncryptedCookie key sessionCookieName session (Cookie.MaxAge oneWeek)
  where
    oneWeek = 3600 * 24 * 7

oauth2StateCookieHeader :: ClientSession.Key -> AuthorizeState -> IO SetCookie
oauth2StateCookieHeader key authorizeState =
  Cookie.setEncryptedCookie key oauth2StateCookieName authorizeState (Cookie.MaxAge oneHour)
  where
    oneHour = 3600
