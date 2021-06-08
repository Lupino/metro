{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Metro.Example.Device
  ( DeviceT
  , DeviceEnv
  , initDeviceEnv
  , sessionGen
  , sessionHandler
  , runDeviceT
  , startDeviceT
  , request
  ) where

import           Data.ByteString     (ByteString)
import           Data.Default.Class  (def)
import           Data.Word           (Word16)
import           Metro               (ConnEnv, NodeEnv1, NodeT, SessionT,
                                      Transport, initEnv1, makeResponse_,
                                      runNodeT1, startNodeT)
import qualified Metro               as N (request)
import           Metro.Example.Types
import           UnliftIO

type DeviceT = NodeT () ByteString Word16 Packet

type DeviceEnv tp = NodeEnv1 () ByteString Word16 Packet tp

initDeviceEnv :: MonadIO m => ConnEnv tp -> ByteString -> m (DeviceEnv tp)
initDeviceEnv connEnv nid = do
  gen <- liftIO sessionGen
  initEnv1 id connEnv () nid True gen

sessionGen :: IO (IO Word16)
sessionGen = do
  gen <- newTVarIO 1
  return $ atomically $ do
    v <- readTVar gen
    writeTVar gen $! (if v == maxBound then 1 else v + 1)
    return v

sessionHandler
  :: (MonadUnliftIO m, Transport tp)
  => SessionT () ByteString Word16 Packet tp m ()
sessionHandler = makeResponse_ . const $ Just def {packetCmd = Data "pong"}

runDeviceT :: Monad m => DeviceEnv tp -> DeviceT tp m a -> m a
runDeviceT  = runNodeT1

startDeviceT :: (MonadUnliftIO m, Transport tp) => DeviceEnv tp -> m ()
startDeviceT env = runDeviceT env $ startNodeT sessionHandler

request :: (MonadUnliftIO m, Transport tp) => Command -> DeviceT tp m (Maybe Command)
request cmd = fmap packetCmd <$> N.request Nothing def { packetCmd = cmd }
