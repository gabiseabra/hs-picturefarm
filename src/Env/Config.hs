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

data Environment = Production | Development | Test deriving (Show)

data Config = Config
  { port               :: Int
  , host               :: String
  , databaseUrl        :: String
  , cdnCloudName       :: String
  , cdnUploadPreset    :: String
  , cdnApiKey          :: String
  , cdnApiSecret       :: String
  , slackSigningSecret :: String
  , slackClientId      :: String
  , slackClientSecret  :: String
  , slackScopes        :: String
  } deriving (Generic, Show)

instance FromEnv Config

instance Default Config where
  def = Config
    { port               = 4000
    , host               = "https://localhost"
    , databaseUrl        =
      "postgres://postgres:postgres@localhost:5432/picturefarm_dev?"
        <> "sslmode=disable"
    , cdnCloudName       = ""
    , cdnUploadPreset    = ""
    , cdnApiKey          = ""
    , cdnApiSecret       = ""
    , slackSigningSecret = ""
    , slackClientId      = ""
    , slackClientSecret  = ""
    , slackScopes        = ""
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
envFileName (Just Production) = ".env"
envFileName (Just Test      ) = ".env.test"
envFileName _                 = ".env.dev"

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
