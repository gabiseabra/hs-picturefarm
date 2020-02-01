module Config.Env
  ( EnvException
  , Config(..)
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
import           Control.Monad.IO.Class         ( liftIO )

import           LoadEnv                        ( loadEnvFrom )
import           System.Envy                    ( FromEnv
                                                , decodeEnv
                                                )
import           System.Environment             ( lookupEnv )

-- Exceptions
----------------------------------------------------------------------

data EnvException =
  DecodeException String
  deriving Show

instance Exception EnvException

-- Config schema
----------------------------------------------------------------------

data Config = Config {
  databaseUrl :: String
} deriving (Generic, Show)

instance FromEnv Config

-- Config methods
----------------------------------------------------------------------

envFileName :: IO String
envFileName = do
  env <- lookupEnv "STACK_ENV"
  case env of
    Just "test" -> return ".env.test"
    Just "dev"  -> return ".env.dev"
    _           -> return ".env"

loadConfig :: IO Config
loadConfig = do
  _   <- envFileName >>= loadEnvFrom
  env <- decodeEnv :: IO (Either String Config)
  case env of
    Left  err    -> throw (DecodeException err)
    Right config -> return config
