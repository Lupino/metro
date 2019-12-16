{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Metro.Example
  ( ServerConfig
  , startMetroServer
  , ClientConfig
  , startMetroClient
  ) where

import           Control.Monad             (void)
import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))
import           Data.Default.Class        (def)
import           Metro                     (NodeMode (Multi), initConnEnv,
                                            runConnT)
import           Metro.Conn                (close, receive, send)
import           Metro.Example.Device      (initDeviceEnv, sessionGen,
                                            sessionHandler, startDeviceT)
import           Metro.Example.Types       (Command (..), Packet (..))
import           Metro.Example.Web         (startWeb)
import           Metro.Server              (ServerEnv (..), initServerEnv,
                                            startServer)
import           Metro.Transport.Socket    (rawSocket, socketUri)
import           System.IO                 (stderr)
import           System.Log                (Priority (..))
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger
import           UnliftIO.Concurrent       (forkIO)

data ServerConfig = ServerConfig
  { webHost   :: String
  , webPort   :: Int
  , sockPort  :: String
  , keepalive :: Int
  , sessTout  :: Int
  , logLevel  :: Priority
  }

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o -> do
    webHost   <- o .: "web_host"
    webPort   <- o .: "web_port"
    sockPort  <- o .: "socket"
    keepalive <- o .: "keepalive"
    sessTout  <- o .: "session_timeout"
    logLevel  <- read <$> o .:? "log_level" .!= "ERROR"
    return ServerConfig{..}

startMetroServer :: ServerConfig -> IO ()
startMetroServer ServerConfig {..} = do
  removeAllHandlers
  handle <- streamHandler stderr logLevel >>= \lh -> return $
          setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (addHandler handle . setLevel logLevel)

  gen <- sessionGen
  sEnv <- initServerEnv Multi "Example" sockPort (fromIntegral keepalive) (fromIntegral sessTout) gen $ \_ connEnv -> do
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
