{-# LANGUAGE OverloadedStrings #-}

module Metro.Example.Web
  ( startWeb
  ) where

import           Data.Aeson                      (object, (.=))
import           Data.ByteString                 (ByteString)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Data.Default.Class              (def)
import           Data.Streaming.Network.Internal (HostPreference (Host))
import           Metro.Example.Device            (DeviceEnv, request,
                                                  runDeviceT)
import           Metro.IOHashMap                 (IOHashMap)
import qualified Metro.IOHashMap                 as HM (lookup)
import           Metro.Transport                 (Transport)
import           Network.HTTP.Types              (status500)
import           Network.Wai.Handler.Warp        (setHost, setPort)
import           UnliftIO
import           Web.Scotty                      (ActionM, ScottyM, body, json,
                                                  param, post, raw, scottyOpts,
                                                  settings, status)

startWeb :: (Transport tp) => IOHashMap ByteString (DeviceEnv tp) -> String -> Int -> IO ()
startWeb devicesEnv host port =
  scottyOpts opts $ application devicesEnv
  where opts = def {settings = setPort port $ setHost (Host host) (settings def)}

application :: (Transport tp) => IOHashMap ByteString (DeviceEnv tp) -> ScottyM ()
application devicesEnv =
  post "/api/request/:uuid/" $ requestHandler devicesEnv

requestHandler :: (Transport tp) => IOHashMap ByteString (DeviceEnv tp) -> ActionM ()
requestHandler devicesEnv = do
  ip <- param "uuid"
  wb <- body
  env0 <- HM.lookup devicesEnv ip
  case env0 of
    Nothing -> do
      status status500
      json $ object [ "err" .= ("设备不在线" :: String) ]
    Just env1 -> do
      r <- liftIO $ runDeviceT env1 $ request $ toStrict wb

      case r of
        Nothing -> do
          status status500
          json $ object [ "err" .= ("设备不在线" :: String) ]
        Just v -> raw $ fromStrict v
