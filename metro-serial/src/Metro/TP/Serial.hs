{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Metro.TP.Serial
  ( Serial
  , serial
  , CommSpeed (..)
  , SerialPortSettings (..)
  , defaultSerialSettings
  , withSerial
  ) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B (drop, length, take)
import           Metro.Class                (Transport (..))
import           System.Hardware.Serialport (CommSpeed (..), SerialPort,
                                             SerialPortSettings (..),
                                             defaultSerialSettings)
import qualified System.Hardware.Serialport as S (closeSerial, openSerial, recv,
                                                  send, withSerial)

data Serial = Serial String SerialPort

sendAll :: SerialPort -> ByteString -> IO ()
sendAll _ "" = pure ()
sendAll port bs = do
  r <- S.send port $ B.take 8192 bs
  if r > 0 then sendAll port $! B.drop r bs
           else pure ()

recv :: SerialPort -> Int -> IO ByteString
recv port nbytes = do
  bs <- S.recv port nbytes
  if B.length bs == 0 then recv port nbytes
                      else return bs

instance Transport Serial where
  data TransportConfig Serial
    = SerialPath String SerialPortSettings
    | SerialPort String SerialPort
  newTP (SerialPath port settings) = Serial port <$> S.openSerial port settings
  newTP (SerialPort port soc)      = pure $ Serial port soc
  recvData (Serial _ soc)       = recv soc
  sendData (Serial _ soc)       = sendAll soc
  closeTP  (Serial _ soc)       = S.closeSerial soc
  getTPName (Serial port _)     = pure port

serial :: String -> SerialPortSettings -> TransportConfig Serial
serial = SerialPath

withSerial :: FilePath -> SerialPortSettings -> (TransportConfig Serial -> IO a) -> IO a
withSerial port settings f = S.withSerial port settings $ f . SerialPort port
