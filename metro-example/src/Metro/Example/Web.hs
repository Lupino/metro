{-# LANGUAGE OverloadedStrings #-}

module Metro.Example.Web
  ( startWeb
  ) where

import           Data.Aeson                      (object, (.=))
import           Data.ByteString                 (ByteString)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Data.Default.Class              (def)
import           Data.IOMap                      (IOMap)
import qualified Data.IOMap                      as Map (lookup)
import           Data.Streaming.Network.Internal (HostPreference (Host))
import           Metro                           (Transport)
import           Metro.Example.Device            (DeviceEnv, request,
                                                  runDeviceT)
import           Metro.Example.Types             (Command (..), File (..))
import           Network.HTTP.Types              (status500)
import           Network.Wai.Handler.Warp        (setHost, setPort)
import           UnliftIO
import           Web.Scotty                      (ActionM, ScottyM, body, get,
                                                  json, param, post, put, raw,
                                                  scottyOpts, settings, status)

startWeb :: (Transport tp) => IOMap ByteString (DeviceEnv tp) -> String -> Int -> IO ()
startWeb devicesEnv host port =
  scottyOpts opts $ application devicesEnv
  where opts = def {settings = setPort port $ setHost (Host host) (settings def)}

application :: (Transport tp) => IOMap ByteString (DeviceEnv tp) -> ScottyM ()
application devicesEnv = do
  post "/api/run/:uuid/" $ requestHandler devicesEnv
  get "/api/download/:uuid/" $ downloadHandler devicesEnv
  put "/api/upload/:uuid/" $ uploadHandler Upload devicesEnv
  put "/api/append/:uuid/" $ uploadHandler Append devicesEnv
  post "/api/end/:uuid/" $ endHandler devicesEnv

requestHandler :: (Transport tp) => IOMap ByteString (DeviceEnv tp) -> ActionM ()
requestHandler devicesEnv = do
  ip <- param "uuid"
  wb <- body
  env0 <- Map.lookup ip devicesEnv
  case env0 of
    Nothing -> do
      status status500
      json $ object [ "err" .= ("Device is offline" :: String) ]
    Just env1 -> do
      r <- liftIO $ runDeviceT env1 $ request $ Run $ toStrict wb

      responseCmd r

uploadHandler
  :: (Transport tp)
  => (File -> Command)
  -> IOMap ByteString (DeviceEnv tp) -> ActionM ()
uploadHandler cmd devicesEnv = do
  ip <- param "uuid"
  fn <- param "fileName"
  wb <- body
  env0 <- Map.lookup ip devicesEnv
  case env0 of
    Nothing -> do
      status status500
      json $ object [ "err" .= ("Device is offline" :: String) ]
    Just env1 -> do
      r <- liftIO $ runDeviceT env1 $ request $ cmd $ File fn $ toStrict wb

      responseCmd r

downloadHandler :: (Transport tp) => IOMap ByteString (DeviceEnv tp) -> ActionM ()
downloadHandler devicesEnv = do
  ip <- param "uuid"
  fn <- param "fileName"
  env0 <- Map.lookup ip devicesEnv
  case env0 of
    Nothing -> do
      status status500
      json $ object [ "err" .= ("Device is offline" :: String) ]
    Just env1 -> do
      r <- liftIO $ runDeviceT env1 $ request $ Download fn

      responseCmd r

endHandler :: (Transport tp) => IOMap ByteString (DeviceEnv tp) -> ActionM ()
endHandler devicesEnv = do
  ip <- param "uuid"
  env0 <- Map.lookup ip devicesEnv
  case env0 of
    Nothing -> do
      status status500
      json $ object [ "err" .= ("Device is offline" :: String) ]
    Just env1 -> do
      r <- liftIO $ runDeviceT env1 $ request End

      responseCmd r

responseCmd :: Maybe Command -> ActionM ()
responseCmd (Just (Data v)) = raw $ fromStrict v
responseCmd (Just _) = do
  status status500
  json $ object [ "err" .= ("Wrong response" :: String) ]
responseCmd Nothing = do
  status status500
  json $ object [ "err" .= ("Device is offline" :: String) ]
