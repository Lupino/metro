{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Metro.Example
  ( ServerConfig
  , startMetroServer
  , startMetroClient
  ) where

import           Control.Monad             (forever, void)
import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))
import           Data.ByteString           (ByteString)
import           Data.Default.Class        (def)
import           Data.List                 (isPrefixOf)
import           Data.Word                 (Word16)
import           Metro                     (NodeMode (..), SessionMode (..),
                                            request, withSessionT)
import           Metro.Class               (Servable (STP),
                                            Transport (TransportConfig))
import qualified Metro.Class               as S (Servable (ServerConfig))
import           Metro.Conn                (close, initConnEnv, receive,
                                            runConnT)
import           Metro.Example.Device      (DeviceEnv, initDeviceEnv,
                                            runDeviceT, sessionGen,
                                            sessionHandler, startDeviceT)
import           Metro.Example.Types       (Command (..), Packet (..))
import           Metro.Example.Web         (startWeb)
import           Metro.Server              (ServerEnv, getNodeEnvList,
                                            initServerEnv,
                                            setDefaultSessionTimeout,
                                            setKeepalive, setNodeMode,
                                            setServerName, setSessionMode,
                                            startServer)
import           Metro.Session             (send)
import           Metro.TCP                 (tcpConfig)
import           Metro.Transport.Debug     (DebugMode (..), debugConfig)
import           Metro.Transport.Socket    (socketUri)
import           Metro.Transport.UDPSocket (udpSocket)
import           Metro.UDP                 (udpConfig)
import           Metro.Utils               (setupLog)
import           System.Log                (Priority (..))
import           UnliftIO                  (liftIO)
import           UnliftIO.Concurrent       (forkIO, threadDelay)

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
  -> S.ServerConfig serv
  -> (ExampleEnv serv tp -> ExampleEnv serv tp)
  -> IO (ExampleEnv serv tp)
newMetroServer mk config mapEnv = do
  gen <- sessionGen
  sEnv <- fmap mapEnv . initServerEnv config gen mk $ \_ connEnv -> do
    cmd <- packetCmd <$> runConnT connEnv receive
    case cmd of
      Data nid -> return $ Just (nid, ())
      _        -> do
        runConnT connEnv close
        return Nothing
  void $ forkIO $ startServer sEnv sessionHandler
  return sEnv

startMetroServer :: ServerConfig -> IO ()
startMetroServer ServerConfig {..} = do
  setupLog logLevel
  if "udp" `isPrefixOf` sockPort then do
    sEnv <- newMetroServer (debugConfig "Example" Raw) (udpConfig sockPort) mapEnv
    startWeb (getNodeEnvList sEnv) webHost webPort
  else do
    sEnv <- newMetroServer (debugConfig "Example" Raw) (tcpConfig sockPort) mapEnv
    startWeb (getNodeEnvList sEnv) webHost webPort

  where mapEnv :: ExampleEnv serv tp -> ExampleEnv serv tp
        mapEnv =
          setNodeMode Multi
          . setSessionMode SingleAction
          . setKeepalive (fromIntegral keepalive)
          . setDefaultSessionTimeout (fromIntegral sessTout)
          . setServerName "Example"

startMetroClient :: ServerConfig -> IO ()
startMetroClient ServerConfig {..} = do
  setupLog logLevel
  if "udp" `isPrefixOf` sockPort then
    runMetroClient $ udpSocket sockPort
  else
    runMetroClient $ socketUri sockPort

runMetroClient :: Transport tp => TransportConfig tp -> IO ()
runMetroClient config = do
    connEnv <- initConnEnv (debugConfig "ExampleClient" Raw config)
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
