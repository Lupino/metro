{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Metro.UDP
  ( UDPServer
  , udpConfig
  , newClient
  ) where

import           Data.ByteString           (ByteString, empty)
import           Data.Hashable
import           Data.Maybe                (listToMaybe)
import           Metro.Class               (GetPacketId, RecvPacket)
import           Metro.Conn
import           Metro.IOHashMap           (IOHashMap, newIOHashMap)
import qualified Metro.IOHashMap           as HM (delete, insert, lookup)
import           Metro.Node                (NodeEnv1)
import           Metro.Session             (SessionT)
import           Metro.Transport           (Transport, TransportConfig)
import           Metro.Transport.BS        (BSHandle, BSTransport,
                                            bsTransportConfig, feed,
                                            newBSHandle)
import           Network.Socket            (AddrInfo (..), AddrInfoFlag (..),
                                            SockAddr, Socket,
                                            SocketOption (ReuseAddr),
                                            SocketType (..), addrAddress,
                                            addrFlags, addrSocketType, bind,
                                            defaultHints, getAddrInfo,
                                            setSocketOption, socket)
import qualified Network.Socket            as Socket (close)
import           Network.Socket.ByteString (recvFrom, sendAllTo)
import           System.Log.Logger         (errorM)
import           UnliftIO

import           Metro.Servable            (Servable (..), ServerT, getServ,
                                            handleConn, serverEnv)

data UDPServer = UDPServer Socket (IOHashMap String BSHandle)

instance Servable UDPServer where
  data ServConfig UDPServer = UDPConfig String
  type ServID UDPServer = SockAddr
  type STP UDPServer = BSTransport
  newServ (UDPConfig hostPort) = do
    sock <- liftIO $ bindTo hostPort
    handleList <- newIOHashMap
    return $ UDPServer sock handleList
  servOnce us@(UDPServer serv handleList) = do
    (bs, addr) <- liftIO $ recvFrom serv 1024

    bsHandle <- HM.lookup handleList $ show addr
    case bsHandle of
      Just h  -> feed h bs >> return Nothing
      Nothing -> do
        config <- newTransportConfig us addr bs
        return $ Just (addr, config)

  onConnEnter _ _ = return ()
  onConnLeave (UDPServer _ handleList) addr = HM.delete handleList (show addr)
  close (UDPServer serv _) = liftIO $ Socket.close serv

udpConfig :: String -> ServConfig UDPServer
udpConfig = UDPConfig

-- Returns the first action from a list which does not throw an exception.
-- If all the actions throw exceptions (and the list of actions is not empty),
-- the last exception is thrown.
-- The operations are run outside of the catchIO cleanup handler because
-- catchIO masks asynchronous exceptions in the cleanup handler.
-- In the case of complete failure, the last exception is actually thrown.
firstSuccessful :: [IO a] -> IO a
firstSuccessful = go Nothing
  where
  -- Attempt the next operation, remember exception on failure
  go _ (p:ps) =
    do r <- tryIO p
       case r of
         Right x -> return x
         Left  e -> go (Just e) ps

  -- All operations failed, throw error if one exists
  go Nothing  [] = error "firstSuccessful: empty list"
  go (Just e) [] = throwIO e

getDatagramAddrList :: String -> IO [AddrInfo]
getDatagramAddrList hostPort = getAddrInfo (Just hints) host port
  where hints = defaultHints
          { addrFlags = [AI_PASSIVE]
          , addrSocketType = Datagram
          }

        host = getHost hostPort
        port = getService hostPort

getDatagramAddr :: String -> IO (Maybe AddrInfo)
getDatagramAddr hostPort = listToMaybe <$> getDatagramAddrList hostPort

bindTo :: String -> IO Socket
bindTo hostPort = do
  addrs <- getDatagramAddrList hostPort
  firstSuccessful $ map tryToConnect addrs
  where
  tryToConnect addr =
    bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        Socket.close  -- only done if there's an error
        (\sock -> do
          setSocketOption sock ReuseAddr 1
          bind sock $ addrAddress addr
          return sock
        )

newTransportConfig
  :: (MonadIO m)
  => UDPServer
  -> SockAddr
  -> ByteString
  -> m (TransportConfig BSTransport)
newTransportConfig (UDPServer sock handleList) addr bs = do
  h <- newBSHandle bs
  HM.insert handleList (show addr) h
  return $ bsTransportConfig h $ flip (sendAllTo sock) addr

newClient
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt)
  => (TransportConfig BSTransport -> TransportConfig tp)
  -> String
  -> nid
  -> u
  -> SessionT u nid k rpkt tp m ()
  -> ServerT UDPServer u nid k rpkt tp m (Maybe (NodeEnv1 u nid k rpkt tp))
newClient mk hostPort nid uEnv sess = do
  addr <- liftIO $ getDatagramAddr hostPort
  case addr of
    Nothing -> do
      liftIO $ errorM "Metro.UDP" $ "Connect UDP Server " ++ hostPort ++ " failed"
      return Nothing
    Just addr0 -> do
      us <- getServ <$> serverEnv
      config <- mk <$> newTransportConfig us (addrAddress addr0) empty
      connEnv <- initConnEnv config
      Just <$> handleConn "Server" (addrAddress addr0) connEnv nid uEnv sess

dropS :: String -> String
dropS = drop 3 . dropWhile (/= ':')

toMaybe :: String -> Maybe String
toMaybe [] = Nothing
toMaybe xs = Just xs

getHost :: String -> Maybe String
getHost = toMaybe . takeWhile (/=':') . dropS

getService :: String -> Maybe String
getService = toMaybe . drop 1 . dropWhile (/=':') . dropS
