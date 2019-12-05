{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Metro.Transport.Debug
  ( Debug
  , debugConfig
  ) where

import           Metro.Transport

data Debug tp = Debug tp

instance Transport tp => Transport (Debug tp) where
  data TransportConfig (Debug tp) = DebugConfig (TransportConfig tp)
  newTransport (DebugConfig config) = do
    tp <- newTransport config
    return $ Debug tp

  recvData (Debug tp) nbytes = do
    bs <- recvData tp nbytes
    print $ "recv " ++ show bs
    return bs
  sendData (Debug tp) bs = do
    print $ "send " ++ show bs
    sendData tp bs
  closeTransport (Debug tp) = closeTransport tp

debugConfig :: TransportConfig tp -> TransportConfig (Debug tp)
debugConfig = DebugConfig
