module ZoomHub.Web.Types.EmbedConstraint
  ( EmbedConstraint (..),
  )
where

import Data.Bifunctor (first)
import qualified Data.Text as T
import Servant (FromHttpApiData, parseUrlPiece)
import ZoomHub.Web.Types.OpenSeadragonViewerConfig (Constraint)
import qualified ZoomHub.Web.Types.OpenSeadragonViewerConfig as Constraint

-- Type
newtype EmbedConstraint = EmbedConstraint {unEmbedConstraint :: Constraint}
  deriving (Eq, Show)

parse :: String -> Either String EmbedConstraint
parse value =
  case value of
    "zoom" -> Right $ EmbedConstraint Constraint.Zoom
    "full" -> Right $ EmbedConstraint Constraint.Full
    s -> Left $ "Invalid value: " <> s

-- Text
instance FromHttpApiData EmbedConstraint where
  parseUrlPiece p = first T.pack $ parse . T.unpack $ p
