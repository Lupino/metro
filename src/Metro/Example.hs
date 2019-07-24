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
import           Metro.Conn             (initConnEnv, receive, runConnT, send)
import           Metro.Example.Device   (initDeviceEnv, sessionGen,
                                         sessionHandler, startDeviceT)
import           Metro.Example.Types    (Packet (..))
import           Metro.Example.Web      (startWeb)
import           Metro.Server           (ServerEnv (..), initServerEnv,
                                         startServer)
import           Metro.Socket           (listen)
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
  sock <- listen sockPort
  gen <- sessionGen
  sEnv <- initServerEnv sock (fromIntegral keepalive) () gen $ \_ connEnv ->
    packetBody <$> runConnT connEnv receive
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
    , packetBody = "metro-client"
    }
  env0 <- initDeviceEnv connEnv "metro-client"
  startDeviceT env0
