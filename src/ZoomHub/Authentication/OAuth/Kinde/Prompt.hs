{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Authentication.OAuth.Kinde.Prompt
  ( Prompt (..),
    toByteString,
  )
where

import Data.ByteString (ByteString)

data Prompt = Login | Create

toByteString :: Prompt -> ByteString
toByteString = \case
  Login -> "login"
  Create -> "create"
