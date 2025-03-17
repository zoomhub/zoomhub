{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZoomHub.Types.User where

import Data.Text (Text)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL (FromPG (fromPG), IsPG, ToPG)

-- user
data User = User
  { kindeUserId :: !Text,
    email :: !Email,
    isEmailVerified :: !Bool,
    givenName :: !(Maybe Text),
    familyName :: !(Maybe Text),
    imageURL :: !(Maybe Text)
  }
  deriving (Eq, GHC.Generic, Show)

-- PostgreSQL / Squeal
instance SOP.Generic User

instance SOP.HasDatatypeInfo User

-- email
newtype Email = Email Text
  deriving (Eq, GHC.Generic, Show)
  deriving newtype (IsPG, ToPG db)

-- PostgreSQL / Squeal
instance SOP.Generic Email

instance SOP.HasDatatypeInfo Email

instance FromPG Email where
  fromPG = Email <$> (fromPG @Text)
