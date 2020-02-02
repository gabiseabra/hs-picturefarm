module Env
  ( module Env.Config
  , module Env.Connection
  , AppContext
  , Env(..)
  , EnvM(..)
  , initialize
  , runEnvIO
  )
where

import           Env.Config
import           Env.Connection


import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , runReaderT
                                                )
import           Control.Monad.Trans            ( MonadIO
                                                , MonadTrans
                                                , liftIO
                                                )

import           Data.Pool                      ( takeResource )

-- Application environment
----------------------------------------------------------------------

type AppContext = (Config, Pool Connection)

data Env = Env {
  conn   :: Connection,
  config :: Config
}

newtype EnvM a
  = EnvM { runEnvM :: ReaderT Env IO a }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env
    )


----------------------------------------------------------------------

initialize :: IO AppContext
initialize = do
  config <- loadConfig
  pool   <- createConnectionPool config
  return (config, pool)

getEnv :: AppContext -> IO Env
getEnv (config, pool) = do
  (conn, _) <- takeResource pool
  return Env { conn, config }

runEnvIO :: AppContext -> EnvM a -> IO a
runEnvIO ctx m = do
  env <- getEnv ctx
  runReaderT (runEnvM m) env
