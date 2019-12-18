{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Metro.Example
  ( ServerConfig
  , startMetroServer
  , startMetroClient
  ) where

import           Control.Monad          (forever, void)
import           Data.Aeson             (FromJSON, parseJSON, withObject, (.!=),
                                         (.:), (.:?))
import           Data.ByteString        (ByteString)
import           Data.Default.Class     (def)
import           Data.List              (isPrefixOf)
import           Data.Word              (Word16)
import           Metro                  (NodeMode (..), request, withSessionT)
import           Metro.Conn             (close, initConnEnv, receive, runConnT)
import           Metro.Example.Device   (DeviceEnv, initDeviceEnv, runDeviceT,
                                         sessionGen, sessionHandler,
                                         startDeviceT)
import           Metro.Example.Types    (Command (..), Packet (..))
import           Metro.Example.Web      (startWeb)
import           Metro.Servable         (Servable (STP, ServConfig), ServerEnv,
                                         getNodeEnvList, initServerEnv,
                                         runServerT, startServer, stopServerT)
import           Metro.Session          (send)
import           Metro.TCP              (tcpConfig)
import           Metro.Transport        (Transport, TransportConfig)
import           Metro.Transport.Debug  (DebugMode (..), debugConfig)
import           Metro.Transport.Socket (socketUri)
import           Metro.UDP              (newClient, udpConfig)
import           Metro.Utils            (setupLog)
import           System.Log             (Priority (..))
import           UnliftIO               (liftIO)
import           UnliftIO.Concurrent    (forkIO, threadDelay)

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

type ExampleEnv serv = ServerEnv serv () ByteString Word16 Packet

newMetroServer
  :: (Servable serv, Transport tp)
  => (TransportConfig (STP serv) -> TransportConfig tp)
  -> NodeMode -> ServConfig serv -> Int -> Int -> IO (ExampleEnv serv tp)
newMetroServer mk mode config keepalive sessTout = do
  gen <- sessionGen
  sEnv <- initServerEnv mode "Example" config (fromIntegral keepalive) (fromIntegral sessTout) gen $ \_ connEnv -> do
    cmd <- packetCmd <$> runConnT connEnv receive
    case cmd of
      Data nid -> return $ Just (nid, ())
      _        -> do
        runConnT connEnv close
        return Nothing
  void $ forkIO $ startServer sEnv mk sessionHandler
  return sEnv

startMetroServer :: ServerConfig -> IO ()
startMetroServer ServerConfig {..} = do
  setupLog logLevel
  if "udp" `isPrefixOf` sockPort then do
    sEnv <- newMetroServer (debugConfig "Example" Raw) Multi (udpConfig sockPort) keepalive sessTout
    startWeb (getNodeEnvList sEnv) webHost webPort
  else do
    sEnv <- newMetroServer (debugConfig "Example" Raw) Multi (tcpConfig sockPort) keepalive sessTout
    startWeb (getNodeEnvList sEnv) webHost webPort

startMetroClient :: ServerConfig -> IO ()
startMetroClient ServerConfig {..} = do
  setupLog logLevel
  if "udp" `isPrefixOf` sockPort then do
    sEnv <- newMetroServer (debugConfig "ExampleClient" Raw) Multi (udpConfig "udp://:0") keepalive sessTout
    env0 <- runServerT sEnv $ newClient (debugConfig "ExampleClient" Raw) sockPort "client" () sessionHandler
    case env0 of
      Nothing   -> do
        putStrLn "error"
        runServerT sEnv stopServerT
      Just env1 -> runAction env1
  else do
    connEnv <- initConnEnv (debugConfig "ExampleClient" Raw $ socketUri sockPort)
    env0 <- initDeviceEnv connEnv "metro-client"
    void . forkIO $ startDeviceT env0
    runAction env0

runAction :: (Transport tp) => DeviceEnv tp -> IO ()
runAction env0 = runDeviceT env0 $ do
  withSessionT Nothing $ send $ def
    { packetId = 0
    , packetCmd = Data "metro-client"
    }

  forever $ do
    pkt <- request Nothing $ def
      { packetId = 0
      , packetCmd = Data "metro-client"
      }

    case pkt of
      Nothing            -> liftIO $ putStrLn "error: metro-client"
      Just (p :: Packet) -> liftIO $ print p

    threadDelay 10000000
