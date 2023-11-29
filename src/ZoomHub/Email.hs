{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Email
  ( Email (..),
    From (..),
    To (..),
    send,
  )
where

import Control.Lens ((&), (.~), (?~))
import Data.Text (Text)
import qualified Amazonka as AWS
import qualified Amazonka.SES as SES
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
  ZHAWS.run config logLevel $
    AWS.send $
      SES.sendEmail (unFrom from) destination message
  where
    destination = SES.destination & SES.dToAddresses .~ [unTo to]

    message = SES.message content' body'
    content' = SES.content "" & SES.cData .~ subject
    body' = SES.body & SES.bText ?~ SES.content body
