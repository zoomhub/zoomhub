{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.Types.ContentCompletion
  ( ContentCompletion (..),
    SuccessCompletion (..),
    FailureCompletion (..),
  )
where

import Control.Monad ()
import Data.Aeson.Types
  ( (.:),
    FromJSON (parseJSON),
    Object,
    Parser,
    withObject,
  )
import Data.Int (Int64)
import Data.Text (Text)
import ZoomHub.API.Types.DeepZoomImageWithoutURL (DeepZoomImage)
import ZoomHub.Types.ContentMIME (ContentMIME)
import qualified ZoomHub.Types.ContentMIME as ContentMIME

data ContentCompletion
  = Success SuccessCompletion
  | Failure FailureCompletion
  deriving (Eq, Show)

data SuccessCompletion
  = SuccessCompletion
      { scDZI :: DeepZoomImage,
        scSize :: Int64,
        scMIME :: Maybe ContentMIME
      }
  deriving (Eq, Show)

newtype FailureCompletion
  = FailureCompletion
      { fcErrorMessage :: Text
      }
  deriving (Eq, Show)

-- JSON
instance FromJSON SuccessCompletion where
  parseJSON = withObject "SuccessCompletion" parseSuccessCompletion

parseSuccessCompletion :: Object -> Parser SuccessCompletion
parseSuccessCompletion obj = do
  mime <- obj .: "mime"
  size <- obj .: "size"
  dzi <- obj .: "dzi"
  pure $ SuccessCompletion
    { scDZI = dzi,
      scSize = size,
      scMIME = ContentMIME.fromText mime
    }

instance FromJSON FailureCompletion where
  parseJSON = withObject "FailureCompletion" parseFailureCompletion

parseFailureCompletion :: Object -> Parser FailureCompletion
parseFailureCompletion obj = do
  errorMessage <- obj .: "error"
  pure $ FailureCompletion {fcErrorMessage = errorMessage}

instance FromJSON ContentCompletion where
  parseJSON = withObject "ContentCompletion" $ \obj -> do
    type_ <- obj .: "type" :: Parser Text
    case type_ of
      "success" -> Success <$> parseSuccessCompletion obj
      "failure" -> Failure <$> parseFailureCompletion obj
      _ -> fail "failed to parse ContentCompletion"
