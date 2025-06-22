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

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Int (Int64)
import Data.Text (Text)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL (FromPG (fromPG), IsPG, ToPG)
import ZoomHub.Squeal.Citext ()

-- user
data User = User
  { id :: Int64,
    kindeUserId :: !Text,
    email :: !(CI Text),
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
newtype Email = Email (CI Text)
  deriving (Eq, GHC.Generic, Show)
  deriving newtype (IsPG, ToPG db)

-- PostgreSQL / Squeal
instance SOP.Generic Email

instance SOP.HasDatatypeInfo Email

instance FromPG Email where
  fromPG = Email . CI.mk <$> (fromPG @Text)

-- User creation result
data UserCreationResult
  = UserCreated User
  | UserFound User
  deriving (Eq, GHC.Generic, Show)

-- PostgreSQL / Squeal
instance SOP.Generic UserCreationResult

instance SOP.HasDatatypeInfo UserCreationResult
