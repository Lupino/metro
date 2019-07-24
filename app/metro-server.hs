{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Yaml          (decodeFileEither)
import           Metro.Example      (startMetroServer)
import           System.Environment (getArgs)

defaultConfigPath :: String
defaultConfigPath = "config.yml"

main :: IO ()
main = do
  args <- getArgs
  let configPath = case args of
                     []    -> defaultConfigPath
                     (x:_) -> x
  c <- decodeFileEither configPath
  case c of
    Left e       -> putStrLn $ "Load config file " ++ configPath ++ show e
    Right config -> startMetroServer config
