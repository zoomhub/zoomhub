{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Email
  ( Email (..),
    From (..),
    To (..),
    send,
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.SES as SES
import qualified Amazonka.SES.Types as SES
import Control.Lens ((&), (?~))
import Data.Text (Text)
import qualified ZoomHub.AWS as ZHAWS
import qualified ZoomHub.Config.AWS as AWS
import ZoomHub.Log.Logger (LogLevel)

data Email = Email
  { from :: From,
    to :: To,
    subject :: Text,
    body :: Text
  }

newtype From = From {unFrom :: Text}

newtype To = To {unTo :: Text}

send :: AWS.Config -> LogLevel -> Email -> IO SES.SendEmailResponse
send config logLevel Email {..} =
  ZHAWS.run config logLevel $ \env ->
    AWS.send env $ SES.newSendEmail (unFrom from) destination message
  where
    destination = SES.newDestination & SES.destination_toAddresses ?~ [unTo to]

    message = SES.newMessage subject' body'
    subject' = SES.newContent subject
    body' = SES.newBody & SES.body_text ?~ SES.newContent body
