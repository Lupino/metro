module Metro.Socket
  ( Socket
  , close
  , listen
  , connect
  , getHost
  , getService
  -- udp
  , bindTo
  , getDatagramAddr
  ) where

import           Control.Exception (bracketOnError, throwIO)
import           Control.Monad     (when)
import           Data.List         (isPrefixOf)
import           Data.Maybe        (listToMaybe)
import           Network.Socket    hiding (bind, connect, listen)
import qualified Network.Socket    as S (bind, connect, listen)
import           System.Directory  (doesFileExist, removeFile)
import           System.Exit       (exitFailure)
import           UnliftIO          (tryIO)

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


connectTo :: Maybe HostName -> Maybe ServiceName -> IO Socket
connectTo host serv = do
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) host serv
    firstSuccessful $ map tryToConnect addrs
  where
  tryToConnect addr =
    bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close  -- only done if there's an error
        (\sock -> do
          S.connect sock (addrAddress addr)
          return sock
        )

connectToFile :: FilePath -> IO Socket
connectToFile path =
  bracketOnError
    (socket AF_UNIX Stream 0)
    close
    (\sock -> do
      S.connect sock (SockAddrUnix path)
      return sock
    )

listenOnFile :: FilePath -> IO Socket
listenOnFile path =
  bracketOnError
    (socket AF_UNIX Stream 0)
    close
    (\sock -> do
        setSocketOption sock ReuseAddr 1
        S.bind sock (SockAddrUnix path)
        S.listen sock maxListenQueue
        return sock
    )

listenOn :: Maybe HostName -> Maybe ServiceName -> IO Socket
listenOn host serv = do
  -- We should probably specify addrFamily = AF_INET6 and the filter
  -- code below should be removed. AI_ADDRCONFIG is probably not
  -- necessary. But this code is well-tested. So, let's keep it.
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]
                           , addrSocketType = Stream
                           }
  addrs <- getAddrInfo (Just hints) host serv
  -- Choose an IPv6 socket if exists.  This ensures the socket can
  -- handle both IPv4 and IPv6 if v6only is false.
  let addrs' = filter (\x -> addrFamily x == AF_INET6) addrs
      addr = if null addrs' then head addrs else head addrs'
  bracketOnError
      (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
      close
      (\sock -> do
          setSocketOption sock ReuseAddr 1
          setSocketOption sock NoDelay   1
          S.bind sock (addrAddress addr)
          S.listen sock maxListenQueue
          return sock
      )

listen :: String -> IO Socket
listen port =
  if "tcp" `isPrefixOf` port then
    listenOn (getHost port) (getService port)
  else do
    let sockFile = dropS port
    exists <- doesFileExist sockFile
    when exists $ do
      e <- tryIO $ connectToFile sockFile
      case e of
        Left _ -> removeFile sockFile
        Right _ -> do
          putStrLn "periodicd: bind: resource busy (Address already in use)"
          exitFailure
    listenOnFile sockFile

connect :: String -> IO Socket
connect h | "tcp" `isPrefixOf` h = connectTo (getHost h) (getService h)
          | otherwise            = connectToFile (dropS h)

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
        close  -- only done if there's an error
        (\sock -> do
          setSocketOption sock ReuseAddr 1
          S.bind sock $ addrAddress addr
          return sock
        )


-- ipv6 fe80::1046:372a:8c3b:94b8%en0:80

countColon :: String -> Int
countColon = length . filter (==':')

-- ipv6 fe80::1046:372a:8c3b:94b8%en0:80
-- ipv6 fe80::1046:372a:8c3b:94b8%en0
-- ipv4 127.0.0.1:80
-- ipv4 127.0.0.1
-- only port :80
splitHostPort :: String -> (Maybe String, Maybe String)
splitHostPort hostPort
  | colon == 0 = (Just hostPort, Nothing)
  | colon == 1 = (takeFst id hostPort, takeSnd id hostPort)
  | colon == 5 = (Just hostPort, Nothing)
  | colon == 6 = (takeSnd reverse hostPort, takeFst reverse hostPort )
  | otherwise = (Nothing, Nothing)
  where colon = countColon hostPort
        takeFst f = toMaybe . f . takeWhile (/=':') . f
        takeSnd f = toMaybe . f . drop 1 . dropWhile (/=':') . f

dropS :: String -> String
dropS = drop 3 . dropWhile (/= ':')

toMaybe :: String -> Maybe String
toMaybe [] = Nothing
toMaybe xs = Just xs

getHost :: String -> Maybe String
getHost = fst . splitHostPort . dropS

getService :: String -> Maybe String
getService = snd . splitHostPort . dropS
