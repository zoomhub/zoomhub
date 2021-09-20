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
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import qualified ZoomHub.AWS as ZHAWS
import qualified ZoomHub.Config.AWS as AWS

data Email = Email
  { from :: From,
    to :: To,
    subject :: Text,
    body :: Text
  }

newtype From = From {unFrom :: Text}

newtype To = To {unTo :: Text}

send :: AWS.Config -> Email -> IO SES.SendEmailResponse
send config Email {..} =
  ZHAWS.run config $
    AWS.send $
      SES.sendEmail (unFrom from) destination message
  where
    destination = SES.destination & SES.dToAddresses .~ [unTo to]

    message = SES.message content' body'
    content' = SES.content "" & SES.cData .~ subject
    body' = SES.body & SES.bText ?~ SES.content body
