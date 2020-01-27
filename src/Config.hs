module Config where

import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (liftIO)
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.URL
import GHC.Generics
import LoadEnv
import System.Envy

data Config = Config {
  databaseUrl :: [Char]
} deriving (Generic, Show)

instance FromEnv Config

type Init a = ExceptT String IO a

loadConfig :: Init Config
loadConfig = ExceptT $ liftIO $ loadEnv >> decodeEnv

createConnectionsPool :: Config -> Init (Pool Connection)
createConnectionsPool config =
  case parseDatabaseUrl . databaseUrl $ config of
    Just connectionInfo ->
      liftIO $ createPool (connect connectionInfo) close 2 5 10
    _ -> throwError "Invalid database url"

initialize :: Init (Config, Pool Connection)
initialize = do
  config <- loadConfig
  conn <- createConnectionsPool config
  return (config, conn)
