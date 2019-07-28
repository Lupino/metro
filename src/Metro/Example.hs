{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Metro.Example
  ( ServerConfig
  , startMetroServer
  , ClientConfig
  , startMetroClient
  ) where

import           Control.Monad          (void)
import           Data.Aeson             (FromJSON, parseJSON, withObject, (.:))
import           Data.Default.Class     (def)
import           Metro                  (ServerEnv (..), initConnEnv,
                                         initServerEnv, runConnT, startServer)
import           Metro.Conn             (close, receive, send)
import           Metro.Example.Device   (initDeviceEnv, sessionGen,
                                         sessionHandler, startDeviceT)
import           Metro.Example.Types    (Command (..), Packet (..))
import           Metro.Example.Web      (startWeb)
import           Metro.Transport.Socket (rawSocket, socketUri)
import           UnliftIO.Concurrent    (forkIO)

data ServerConfig = ServerConfig
  { webHost   :: String
  , webPort   :: Int
  , sockPort  :: String
  , keepalive :: Int
  }

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o -> do
    webHost   <- o .: "web_host"
    webPort   <- o .: "web_port"
    sockPort  <- o .: "socket"
    keepalive <- o .: "keepalive"
    return ServerConfig{..}

startMetroServer :: ServerConfig -> IO ()
startMetroServer ServerConfig {..} = do
  gen <- sessionGen
  sEnv <- initServerEnv sockPort (fromIntegral keepalive) gen $ \_ connEnv -> do
    cmd <- packetCmd <$> runConnT connEnv receive
    case cmd of
      Data nid -> return $ Just (nid, ())
      _        -> do
        runConnT connEnv close
        return Nothing
  void $ forkIO $ startServer sEnv rawSocket sessionHandler
  startWeb (nodeEnvList sEnv) webHost webPort

newtype ClientConfig = ClientConfig
  { cSockPort :: String
  }

instance FromJSON ClientConfig where
  parseJSON = withObject "ClientConfig" $ \o -> do
    cSockPort     <- o .: "socket"
    return ClientConfig{..}

startMetroClient :: ClientConfig -> IO ()
startMetroClient ClientConfig {..} = do
  connEnv <- initConnEnv (socketUri cSockPort)
  runConnT connEnv $ send $ def
    { packetId = 0
    , packetCmd = Data "metro-client"
    }
  env0 <- initDeviceEnv connEnv "metro-client"
  startDeviceT env0
