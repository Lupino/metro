{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
import           UnliftIO                   (TVar, atomically, newTVarIO,
                                             readTVarIO, writeTVar)

data Serial = Serial
  { portPath       :: String
  , serialPort     :: TVar (Maybe SerialPort)
  , serialSettings :: SerialPortSettings
  }

newSerial :: String -> SerialPortSettings -> Maybe SerialPort -> IO Serial
newSerial portPath serialSettings port = do
  serialPort <- newTVarIO port
  return Serial {..}

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

getSerialPort :: Serial -> IO SerialPort
getSerialPort Serial {..} = do
  mport <- readTVarIO serialPort
  case mport of
    Just port -> return port
    Nothing -> do
      port <- S.openSerial portPath serialSettings
      atomically $ writeTVar serialPort (Just port)
      return port

withSerialPort :: Serial -> (SerialPort -> b -> IO a) -> b -> IO a
withSerialPort s f b = getSerialPort s >>= flip f b

tryCloseSerial :: Serial -> IO ()
tryCloseSerial Serial {..} = do
  mport <- readTVarIO serialPort
  case mport of
    Nothing -> pure ()
    Just port -> do
      S.closeSerial port
      atomically $ writeTVar serialPort Nothing

instance Transport Serial where
  data TransportConfig Serial
    = SerialPath String SerialPortSettings
    | SerialPort String SerialPortSettings SerialPort
  newTP (SerialPath port settings)     = newSerial port settings . Just =<< S.openSerial port settings
  newTP (SerialPort port settings soc) = newSerial port settings (Just soc)
  recvData  s = withSerialPort s recv
  sendData  s = withSerialPort s sendAll
  closeTP     = tryCloseSerial
  getTPName   = pure . portPath

serial :: String -> SerialPortSettings -> TransportConfig Serial
serial = SerialPath

withSerial :: FilePath -> SerialPortSettings -> (TransportConfig Serial -> IO a) -> IO a
withSerial port settings f = S.withSerial port settings $ f . SerialPort port settings
