{-# LANGUAGE OverloadedStrings #-}
module System.Log.Handler.MailMachine where

import Control.Exception (SomeException, catchJust)
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

type SwallowInternalMailMachineExceptions = Bool

mailMachineHandler :: MailQueue ->
                      Subject ->
                      FromAddress ->
                      ReceiversAddresses ->
                      Priority ->
                      SwallowInternalMailMachineExceptions ->
                      IO MailMachineHandler
mailMachineHandler mq s f r p sw = do
    let myWriteFunc mmc msg = do
            let enqueue_ = do
                    _ <- enqueue
                             (mailQueue mmc)
                             (from mmc)
                             (receivers mmc)
                             (subject mmc)
                             (T.pack msg)
                             (T.pack msg)
                    return ()
            -- in general it's "better" to swallow this
            -- particular handler exception (redis connection problem etc.)
            -- and allow other hanlders to still work and log informations
            catchJust
                ((\_ ->
                      if sw
                          then Just ()
                          else Nothing)::(SomeException -> Maybe ()))
                (enqueue_)
                (const . return $ ())
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
