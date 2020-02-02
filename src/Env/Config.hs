module Env.Config
  ( ConfigException(..)
  , Environment(..)
  , Config(..)
  , getEnvironment
  , loadConfig
  )
where

import           GHC.Generics

import           System.Directory               ( getCurrentDirectory )
import           System.IO                      ( IO
                                                , FilePath
                                                )

import           Control.Exception
import           Control.Applicative

import           LoadEnv                        ( loadEnvFrom )
import           System.Envy                    ( FromEnv
                                                , decodeEnv
                                                )
import           System.Environment             ( lookupEnv )

-- Exceptions
----------------------------------------------------------------------

data ConfigException =
  DecodeException String
  deriving Show

instance Exception ConfigException

-- Config schema
----------------------------------------------------------------------

data Environment = Production | Development | Test

data Config = Config {
  databaseUrl :: String,
  port        :: Int
} deriving (Generic, Show)

instance FromEnv Config

-- Config methods
----------------------------------------------------------------------

getEnvironment :: IO (Maybe Environment)
getEnvironment = do
  env <- lookupEnv "STACK_ENV"
  case env of
    Just "test" -> return (Just Test)
    Just "prod" -> return (Just Production)
    Just "dev"  -> return (Just Development)
    _           -> return Nothing

envFileName :: Maybe Environment -> String
envFileName (Just Test       ) = ".env.test"
envFileName (Just Development) = ".env.dev"
envFileName _                  = ".env"

loadConfig :: Maybe Environment -> IO Config
loadConfig env = do
  _   <- loadEnvFrom $ envFileName env
  env <- decodeEnv :: IO (Either String Config)
  case env of
    Left  err    -> throw (DecodeException err)
    Right config -> return config
