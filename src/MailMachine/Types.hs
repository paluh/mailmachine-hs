{-# LANGUAGE OverloadedStrings #-}
module MailMachine.Types
       (connection, queueName, mailQueue, MailQueue, FromAddress,
        ReceiversAddresses, Subject, BodyPlain, BodyHtml)
       where

import           Data.Text        (Text, append)
import           Database.Redis   (Connection)
import           Network.Mail.Mime (Address)

data MailQueue = MailQueue { queueName :: Text
                           , connection :: Connection
                           }

mailQueue :: Text -> Connection -> MailQueue
mailQueue name = MailQueue ("hotqueue:" `append` name)


type FromAddress = Address
type ReceiversAddresses = [Address]
type Subject = Text
type BodyPlain = Text
type BodyHtml = Text
