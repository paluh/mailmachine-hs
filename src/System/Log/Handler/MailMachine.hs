{-# LANGUAGE OverloadedStrings #-}
module System.Log.Handler.MailMachine where

import Data.Text as T
import System.Log (Priority)
import System.Log.Handler.Simple
       (GenericHandler(..), formatter, priority, privData, writeFunc,
        closeFunc)
import System.Log.Formatter (nullFormatter)
import MailMachine
       (enqueue, FromAddress, ReceiversAddresses, MailQueue, Subject)

data MailMachineConfig = MailMachineConfig
    { mailQueue :: MailQueue
    , subject :: Subject
    , from :: FromAddress
    , receivers :: ReceiversAddresses
    }

type MailMachineHandler = GenericHandler MailMachineConfig

mailMachineHandler :: MailQueue -> Subject -> FromAddress -> ReceiversAddresses -> Priority -> IO MailMachineHandler
mailMachineHandler mq s f r p = do
    let myWriteFunc mmc msg = do
            _ <- enqueue
                     (mailQueue mmc)
                     (from mmc)
                     (receivers mmc)
                     (subject mmc)
                     (T.pack msg)
                     (T.pack msg)
            return ()
    return
        GenericHandler {priority =
                                p
                       ,formatter =
                                nullFormatter
                       ,privData =
                                MailMachineConfig mq s f r
                       ,writeFunc =
                                myWriteFunc
                       ,closeFunc =
                                const $
                                return ()}
