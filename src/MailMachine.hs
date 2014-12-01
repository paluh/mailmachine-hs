{-# LANGUAGE OverloadedStrings #-}
module MailMachine (enqueue, mailQueue) where

import           Data.Aeson              (encode)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Text.Lazy          (fromChunks)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Database.Redis          (Reply, rpush, runRedis)
import           MailMachine.Types       (mailQueue, MailQueue, queueName, connection)
import           Network.Mail.Mime       (Address, Mail, addressEmail,
                                          addressName, emptyMail, htmlPart,
                                          mailHeaders, mailParts, mailTo,
                                          plainPart, renderMail')

-- enqueue :: mail_queue subject, body, from_email, recipients, alternatives=None, attachments=None, sent=None):
enqueue :: MailQueue -> Address -> [Address] -> Text -> Text -> Text -> IO (Either Reply Integer)
enqueue mq from to subject plainBody htmlBody = do
  let msg = buildMessage from to subject plainBody htmlBody
      qName = encodeUtf8 . queueName $ mq
      conn = connection mq
  raw <- renderMail' msg
  let serializedMessage = serializeMessage from to raw
  runRedis conn $ rpush qName [B.concat . L.toChunks $ serializedMessage]

showAddress :: Address -> Text
showAddress a =
    fromMaybe "" (addressName a) `T.append` "<" `T.append` addressEmail a `T.append` ">"

serializeMessage :: Address -> [Address] -> L.ByteString -> L.ByteString
serializeMessage from to mail = encode ( showAddress from
                                       , fmap showAddress to
                                       , decodeUtf8 mail
                                       )

buildMessage :: Address -> [Address] -> Text -> Text -> Text -> Mail
buildMessage from to subject plainBody htmlBody =
  (emptyMail from) { mailTo = to
                   , mailHeaders = [("Subject", subject)]
                   , mailParts = [[ plainPart . fromChunks $ [plainBody]
                                  , htmlPart . fromChunks $ [htmlBody]]]
                   }
