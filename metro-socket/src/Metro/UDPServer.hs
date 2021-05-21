{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.UDPServer
  ( UDPServer
  , udpServer
  , getSocket
  ) where


import           Control.Monad       (forever, unless, void)
import           Data.IOMap          (IOMap)
import qualified Data.IOMap          as Map (delete, empty, insert, lookup)
import           Data.String         (fromString)
import           Metro.Class         (Servable (..), TransportConfig)
import           Metro.Socket        (bindTo, getDatagramAddr)
import           Metro.TP.BS         (BSHandle, bsTPConfig, closeBSHandle, feed,
                                      newBSHandle)
import           Metro.TP.UDPSocket  (UDPSocket, doSendAll, recvFrom,
                                      udpSocket_)
import           Network.Socket      (SockAddr, Socket, addrAddress)
import qualified Network.Socket      as Socket (close)
import           System.Log.Logger   (errorM, infoM)
import           UnliftIO
import           UnliftIO.Concurrent (threadDelay)

data Query = Query
  { peer      :: String
  , keepalive :: Int
  , message   :: String
  }
  deriving (Show)

setPeer :: String -> Query -> Query
setPeer s q = q { peer = s }

setKeepalive :: String -> Query -> Query
setKeepalive s q = q { keepalive = read s }

setMessage :: String -> Query -> Query
setMessage s q = q { message = s }

nextParseQuery :: String -> (String -> Query -> Query) -> Query -> Query
nextParseQuery xs f = parseQuery xxs . f val
  where xxs = dropWhile (/='&') xs
        val = takeWhile (/='&') xs

parseQuery :: String -> Query -> Query
parseQuery [] = id
parseQuery ('p':'e':'e':'r':'=':xs) = nextParseQuery xs setPeer
parseQuery ('k':'e':'e':'p':'a':'l':'i':'v':'e':'=':xs) =
  nextParseQuery xs setKeepalive
parseQuery ('m':'e':'s':'s':'a':'g':'e':'=':xs) =
  nextParseQuery xs setMessage
parseQuery (_:xs) = parseQuery xs

data UDPServer = UDPServer (Async ()) Socket (IOMap SockAddr BSHandle)

instance Servable UDPServer where
  data ServerConfig UDPServer = UDPConfig String
  type SID UDPServer = SockAddr
  type STP UDPServer = UDPSocket
  newServer (UDPConfig hostPort) = do
    sock <- liftIO $ bindTo $ takeWhile (/='?') hostPort
    io <- liftIO $ async $
      unless (null $ peer query) $ do
        addrInfo <- getDatagramAddr (peer query)
        case addrInfo of
          Nothing -> errorM "Metro.UDPServer" $ "Connect Peer Server " ++ peer query ++ " failed"
          Just addrInfo0 -> forever $ do
            infoM "Metro.UDPServer" $ "Ping Peer Server " ++ peer query
            doSendAll sock (addrAddress addrInfo0) (fromString $ message query)
            threadDelay (keepalive query * 1000000)

    UDPServer io sock <$> Map.empty
    where query = parseQuery hostPort $ Query "" 600 "message"
  servOnce us@(UDPServer _ serv handleList) done = do
    (bs, addr) <- liftIO $ recvFrom serv 65535

    bsHandle <- Map.lookup addr handleList
    case bsHandle of
      Just h  -> feed h bs
      Nothing ->
        void . async $ do
          h <- newBSHandle bs
          config <- newTransportConfig us addr h
          done $ Just (addr, config)
          closeBSHandle h
          Map.delete addr handleList

  onConnEnter _ _ = return ()
  onConnLeave (UDPServer _ _ handleList) addr = Map.delete addr handleList
  servClose (UDPServer io serv _) = cancel io >> liftIO (Socket.close serv)

udpServer :: String -> ServerConfig UDPServer
udpServer = UDPConfig

newTransportConfig
  :: (MonadIO m)
  => UDPServer
  -> SockAddr
  -> BSHandle
  -> m (TransportConfig UDPSocket)
newTransportConfig (UDPServer _ sock handleList) addr h = do
  Map.insert addr h handleList
  return $ udpSocket_ $ bsTPConfig h (doSendAll sock addr) $ show addr

getSocket :: UDPServer -> Socket
getSocket (UDPServer _ sock _) = sock
