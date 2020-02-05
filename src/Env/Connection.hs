module Env.Connection
  ( Pool
  , Connection
  , ConnectionException
  , createConnection
  , createConnectionPool
  )
where

import           Env.Config

import           System.IO                      ( IO )

import           Control.Exception

import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.URL

-- Exceptions
----------------------------------------------------------------------

data ConnectionException = InvalidDatabaseUrl deriving Show

instance Exception ConnectionException

-- Connection methods
----------------------------------------------------------------------

withConnectInfo :: (ConnectInfo -> IO a) -> Config -> IO a
withConnectInfo fn config = case parseDatabaseUrl . databaseUrl $ config of
  Just connectionInfo -> fn connectionInfo
  _                   -> throwIO InvalidDatabaseUrl

createConnection :: Config -> IO Connection
createConnection = withConnectInfo connect

createConnectionPool :: Config -> IO (Pool Connection)
createConnectionPool = withConnectInfo
  (\connectInfo -> createPool (connect connectInfo) close 2 5 10)
