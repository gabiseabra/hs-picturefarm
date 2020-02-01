module Config
  ( initialize
  , module Config.Env
  , module Config.Connection
  )
where

import           Config.Env
import           Config.Connection

initialize :: IO (Config, Pool Connection)
initialize = do
  config <- loadConfig
  pool   <- createConnectionPool config
  return (config, pool)
