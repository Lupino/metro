{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Metro.Transport.Debug
  ( Debug
  , debugConfig
  ) where

import           Metro.Transport

data Debug tp = Debug String tp

instance Transport tp => Transport (Debug tp) where
  data TransportConfig (Debug tp) = DebugConfig String (TransportConfig tp)
  newTransport (DebugConfig h config) = do
    tp <- newTransport config
    return $ Debug h tp

  recvData (Debug h tp) nbytes = do
    bs <- recvData tp nbytes
    putStrLn $ h ++ " recv " ++ show bs
    return bs
  sendData (Debug h tp) bs = do
    putStrLn $ h ++ " send " ++ show bs
    sendData tp bs
  closeTransport (Debug h tp) = do
    putStrLn $ h ++ " transport close"
    closeTransport tp

debugConfig :: String -> TransportConfig tp -> TransportConfig (Debug tp)
debugConfig = DebugConfig
