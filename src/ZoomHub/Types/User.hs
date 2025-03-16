{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.User where

import Data.Text (Text)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

-- newtype Email = Email Text
--   deriving (Eq, GHC.Generic, Show)

data User = User
  { kindeUserId :: !Text,
    email :: !Text,
    isEmailVerified :: !Bool,
    givenName :: !(Maybe Text),
    familyName :: !(Maybe Text),
    imageURL :: !(Maybe Text)
  }
  deriving (Eq, GHC.Generic, Show)

-- PostgreSQL / Squeal
instance SOP.Generic User

instance SOP.HasDatatypeInfo User
