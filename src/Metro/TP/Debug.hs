{-# LANGUAGE TypeFamilies #-}

module Metro.TP.Debug
  ( Debug
  , DebugMode (..)
  , debugConfig
  ) where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Metro.Class           (Transport (..))
import           System.Log.Logger     (debugM)

hex :: ByteString -> String
hex = Prelude.concatMap w . unpack
  where w ch = let s = "0123456789ABCDEF"
                   x = fromEnum ch
               in [s !! div x 16,s !! mod x 16]

data Debug tp = Debug String (ByteString -> String) tp

data DebugMode = Raw
    | Hex

instance Transport tp => Transport (Debug tp) where
  data TransportConfig (Debug tp) = DebugConfig String DebugMode (TransportConfig tp)
  newTransport (DebugConfig h mode config) = do
    tp <- newTransport config
    return $ Debug h f tp
    where f = case mode of
                Raw -> show
                Hex -> hex

  recvData (Debug h f tp) nbytes = do
    bs <- recvData tp nbytes
    debugM "Metro.Transport.Debug" $ h ++ " recv " ++ f bs
    return bs
  sendData (Debug h f tp) bs = do
    debugM "Metro.Transport.Debug" $ h ++ " send " ++ f bs
    sendData tp bs
  closeTransport (Debug h _ tp) = do
    debugM "Metro.Transport.Debug" $ h ++ " transport close"
    closeTransport tp

debugConfig :: String -> DebugMode -> TransportConfig tp -> TransportConfig (Debug tp)
debugConfig = DebugConfig
