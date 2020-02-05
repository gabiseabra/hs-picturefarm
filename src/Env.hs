{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

type AppContext = (Maybe Environment, Config, Pool Connection)

data Env = Env {
  environment :: Maybe Environment,
  config      :: Config,
  conn        :: Connection
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
  env    <- getEnvironment
  config <- loadConfig env
  pool   <- createConnectionPool config
  return (env, config, pool)

getEnv :: AppContext -> IO Env
getEnv (environment, config, pool) = do
  (conn, _) <- takeResource pool
  return Env { environment, conn, config }

runEnvIO :: AppContext -> EnvM a -> IO a
runEnvIO ctx m = do
  env <- getEnv ctx
  runReaderT (runEnvM m) env
