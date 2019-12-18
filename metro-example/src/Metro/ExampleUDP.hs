{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Metro.ExampleUDP
  ( ServerConfig
  , startMetroServer
  -- , ClientConfig
  , startMetroClient
  ) where

import           Control.Monad             (forever, void)
import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))
import           Data.ByteString           (ByteString)
import           Data.Default.Class        (def)
import           Data.String               (fromString)
import           Data.Word                 (Word16)
import           Metro                     (NodeMode (Multi), request,
                                            runNodeT1)
import           Metro.Example.Device      (sessionGen, sessionHandler)
import           Metro.Example.Types       (Command (..), Packet (..))
import           Metro.Example.Web         (startWeb)
import           Metro.Servable            (ServerEnv (..), getNodeEnvList,
                                            initServerEnv, runServerT,
                                            startServer)
import           Metro.Transport.BS        (BSTransport)
import           Metro.Transport.Debug     (Debug, DebugMode (..), debugConfig)
import           Metro.UDP                 (UDPServer, newClient, udpConfig)
import           System.IO                 (stderr)
import           System.Log                (Priority (..))
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger
import           UnliftIO                  (liftIO)
import           UnliftIO.Concurrent       (forkIO, threadDelay)

data ServerConfig = ServerConfig
  { webHost   :: String
  , webPort   :: Int
  , sockPort  :: String
  , cSockPort :: String
  , keepalive :: Int
  , sessTout  :: Int
  , logLevel  :: Priority
  }

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o -> do
    webHost   <- o .: "web_host"
    webPort   <- o .: "web_port"
    sockPort  <- o .: "socket"
    cSockPort <- o .: "client_socket"
    keepalive <- o .: "keepalive"
    sessTout  <- o .: "session_timeout"
    logLevel  <- read <$> o .:? "log_level" .!= "ERROR"
    return ServerConfig{..}

type ExampleEnv = ServerEnv UDPServer () ByteString Word16 Packet (Debug BSTransport)

newMetroServer :: ServerConfig -> IO ExampleEnv
newMetroServer ServerConfig {..} = do
  removeAllHandlers
  handle <- streamHandler stderr logLevel >>= \lh -> return $
          setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (addHandler handle . setLevel logLevel)

  gen <- sessionGen
  sEnv <- initServerEnv Multi "Example" (udpConfig sockPort) (fromIntegral keepalive) (fromIntegral sessTout) gen $ \addr _ -> do
    print addr
    return $ Just (fromString $ show addr, ())
  void $ forkIO $ startServer sEnv (debugConfig "Example" Raw) sessionHandler
  return sEnv

startMetroServer :: ServerConfig -> IO ()
startMetroServer sc = do
  sEnv <- newMetroServer sc
  startWeb (getNodeEnvList sEnv) (webHost sc) (webPort sc)

startMetroClient :: ServerConfig -> IO ()
startMetroClient sc = do
  sEnv <- newMetroServer sc
  env0 <- runServerT sEnv $ newClient (debugConfig "ExampleClient" Raw) (cSockPort sc) "client" () sessionHandler
  case env0 of
    Nothing -> putStrLn "error"
    Just env1 -> runNodeT1 env1 $ forever $ do
      pkt <- request Nothing $ def
        { packetId = 0
        , packetCmd = Data "metro-client"
        }

      case pkt of
        Nothing            -> liftIO $ putStrLn "error: metro-client"
        Just (p :: Packet) -> liftIO $ print p

      threadDelay 10000000
