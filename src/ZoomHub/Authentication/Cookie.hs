{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Authentication.Cookie
  ( Name (..),
    Header (..),
    empty,
    setEncryptedCookie,
    value,
    MaxAge (..),
  )
where

import Data.Binary (Binary)
import qualified Data.Binary as Binary (decodeOrFail, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Clock (DiffTime)
import Flow
import Web.ClientSession (Key)
import qualified Web.ClientSession as ClientSession
import Web.Cookie (CookiesText, SetCookie (setCookieHttpOnly, setCookieMaxAge, setCookieName, setCookiePath, setCookieSameSite, setCookieSecure, setCookieValue), defaultSetCookie, sameSiteLax)
import Web.HttpApiData (FromHttpApiData)

newtype Name = Name {unName :: ByteString}

newtype Header = Header {unHeader :: Text}
  deriving (FromHttpApiData)

empty :: Name -> SetCookie
empty name =
  defaultSetCookie
    { setCookieName = unName name,
      setCookieValue = "",
      setCookieMaxAge = Just 0,
      setCookiePath = Just "/",
      setCookieSameSite = Just sameSiteLax,
      setCookieHttpOnly = True,
      -- TODO: Support `False` on `localhost` (development) if needed
      setCookieSecure = True
    }

newtype MaxAge = MaxAge DiffTime

setEncryptedCookie ::
  (Binary content) =>
  Key ->
  Name ->
  content ->
  MaxAge ->
  IO SetCookie
setEncryptedCookie key (Name name) content (MaxAge maxAge) = do
  encrypted <- ClientSession.encryptIO key $ BL.toStrict $ Binary.encode content
  pure $
    defaultSetCookie
      { setCookieName = name,
        setCookieValue = Base64URL.encodeBase64 encrypted |> T.encodeUtf8,
        setCookieMaxAge = Just maxAge,
        setCookiePath = Just "/",
        setCookieSameSite = Just sameSiteLax,
        setCookieHttpOnly = True,
        -- TODO: Support `False` on `localhost` (development) if needed:
        setCookieSecure = True
      }

value :: (Binary s) => Key -> Name -> CookiesText -> Maybe s
value key cookieName cookies = mValue
  where
    fromEither = either (const Nothing)
    mValue = do
      value' <- List.lookup (cookieName |> unName |> T.decodeUtf8Lenient) cookies
      e <- fromEither Just $ Base64URL.decodeBase64 (T.encodeUtf8 value')
      x <- ClientSession.decrypt key e
      fromEither (\(_, _, c) -> Just c) $ Binary.decodeOrFail (BL.fromStrict x)
