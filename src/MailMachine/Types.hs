{-# LANGUAGE OverloadedStrings #-}
module MailMachine.Types (connection, queueName, mailQueue, MailQueue) where

import           Data.Text       (append, Text)
import           Database.Redis  (Connection)

data MailQueue = MailQueue { queueName :: Text
                           , connection :: Connection
                           }

mailQueue :: Text -> Connection -> MailQueue
mailQueue name = MailQueue ("hotqueue:" `append` name)
