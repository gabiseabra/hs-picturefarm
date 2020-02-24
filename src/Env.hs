{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env
  ( module Env.Config
  , module Env.Connection
  , AppContext
  , Env(..)
  , EnvM(..)
  , initialize
  , initializeWithDefaults
  , withEnv
  , getEnv
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

import           Data.Pool                      ( takeResource
                                                , withResource
                                                )

-- Application environment
----------------------------------------------------------------------

type AppContext = (Maybe Environment, Config, Pool Connection)

data Env = Env {
  env    :: Maybe Environment,
  config :: Config,
  conn   :: Connection
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

initialize = initializeWithDefaults' Nothing

initializeWithDefaults = initializeWithDefaults' . Just

initializeWithDefaults' :: Maybe Config -> IO AppContext
initializeWithDefaults' defConfig = do
  env    <- getEnvironment
  config <- loadConfigWithDefaults defConfig env
  pool   <- createConnectionPool config
  return (env, config, pool)

getEnv :: AppContext -> IO Env
getEnv (env, config, pool) = do
  (conn, _) <- takeResource pool
  return Env { env, conn, config }

withEnv :: AppContext -> (Env -> IO a) -> IO a
withEnv ctx@(env, config, pool) fn =
  withResource pool (\conn -> fn (Env { env, conn, config }))

runEnvIO :: AppContext -> EnvM a -> IO a
runEnvIO ctx m = withEnv ctx (\env -> runReaderT (runEnvM m) env)
