{-# LANGUAGE ScopedTypeVariables #-}

module Env.Config
  ( ConfigException(..)
  , Environment(..)
  , Config(..)
  , getEnvironment
  , loadConfig
  , loadConfigWithDefaults
  )
where

import           GHC.Generics

import           Data.Default.Class

import           Control.Exception              ( Exception
                                                , throw
                                                )

import           LoadEnv                        ( loadEnvFrom )
import           System.Envy                    ( FromEnv
                                                , decodeEnv
                                                , decodeWithDefaults
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
  port            :: Int,
  databaseUrl     :: String,
  cdnCloudName    :: String,
  cdnUploadPreset :: String,
  cdnApiKey       :: String,
  cdnApiSecret    :: String
} deriving (Generic, Show)

instance FromEnv Config

instance Default Config where
  def = Config
    { port            = 4000
    , databaseUrl     =
      "postgres://postgres:postgres@localhost:5432/picturefarm_dev?"
        <> "sslmode=disable"
    , cdnCloudName    = ""
    , cdnUploadPreset = ""
    , cdnApiKey       = ""
    , cdnApiSecret    = ""
    }

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
loadConfig = loadConfigWithDefaults Nothing

loadConfigWithDefaults :: Maybe Config -> Maybe Environment -> IO Config
loadConfigWithDefaults def env = do
  _                               <- loadEnvFrom $ envFileName env
  configE :: Either String Config <- case def of
    Nothing     -> decodeEnv
    Just config -> fmap Right $ decodeWithDefaults config
  case configE of
    Left  err    -> throw (DecodeException err)
    Right config -> return config
