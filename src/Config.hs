{-# LANGUAGE DeriveGeneric #-}

module Config where

import Control.Monad.Except
import Control.Monad
import System.Environment
import LoadEnv
import System.Envy
import GHC.Generics

data Config = Config {
  databaseUrl :: [Char]
} deriving (Generic, Show)

instance FromEnv Config

type Init a = ExceptT String IO a

loadConfig :: Init Config
loadConfig = ExceptT $ liftIO $ loadEnv >> decodeEnv

initialize :: Init Config
initialize = do
  config <- loadConfig
  return config
