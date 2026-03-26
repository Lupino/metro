{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.TCPServer
  ( TCPServer
  , tcpServer
  ) where

import           Control.Monad      (void, when)
import           Data.Foldable      (for_)
import           Data.List          (isPrefixOf)
import           Metro.Class        (Servable (..))
import           Metro.Socket       (dropScheme, listen)
import           Metro.TP.TCPSocket (TCPSocket, tcpSocket_)
import           Network.Socket     (Socket, SocketOption (KeepAlive), accept,
                                     setSocketOption)
import qualified Network.Socket     as Socket (close)
import           System.Directory   (removeFile)
import           System.Posix.Files (getFileStatus, isSocket)
import           UnliftIO           (async, finally, liftIO, tryAny, tryIO)

data TCPServer = TCPServer Socket (Maybe FilePath)

instance Servable TCPServer where
  data ServerConfig TCPServer = TCPConfig String
  type SID TCPServer = Socket
  type STP TCPServer = TCPSocket
  newServer (TCPConfig hostPort) = liftIO $ do
    serv <- listen hostPort
    pure $ TCPServer serv (socketFile hostPort)
  servOnce (TCPServer serv _) done = do
    (sock, _) <- liftIO $ accept serv
    -- KeepAlive is best effort; some platforms/sockets may reject it.
    _ <- liftIO $ tryAny $ setSocketOption sock KeepAlive 1
    void $ async $ do
      finally
        (done $ Just (sock, tcpSocket_ sock))
        (void $ liftIO $ tryAny $ Socket.close sock)
  onConnEnter _ _ = return ()
  onConnLeave _ _ = return ()
  servClose (TCPServer serv mSocketFile) = liftIO $ do
    void $ tryAny $ Socket.close serv
    for_ mSocketFile removeSocketPath

tcpServer :: String -> ServerConfig TCPServer
tcpServer = TCPConfig

socketFile :: String -> Maybe FilePath
socketFile hostPort
  | "tcp://" `isPrefixOf` hostPort = Nothing
  | otherwise = Just $ dropScheme hostPort

removeSocketPath :: FilePath -> IO ()
removeSocketPath sockFile = do
  eStat <- tryIO $ getFileStatus sockFile
  case eStat of
    Left _ -> pure ()
    Right st ->
      when (isSocket st) $ void $ tryAny $ removeFile sockFile
