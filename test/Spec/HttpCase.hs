module Spec.HttpCase
  ( module Spec.TestCase
  , withMockServer
  , mockHttpConfig
  , mockReq
  )
where

import           Env
import           Spec.TestCase

import           Control.Applicative
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Concurrent             ( forkIO )

import           Data.Maybe                     ( fromMaybe )

import           System.Environment             ( lookupEnv )

import           Network.HTTP.Req               ( HttpConfig(..)
                                                , Req
                                                , defaultHttpConfig
                                                , runReq
                                                )
import           Network.HTTP.Client            ( ManagerSettings(..)
                                                , Request(..)
                                                , BodyReader
                                                , newManager
                                                , defaultManagerSettings
                                                )
import           Web.Scotty                     ( ScottyM
                                                , scottyApp
                                                )
import           Network.Wai.Handler.Warp       ( run )

-- Mock server
----------------------------------------------------------------------

-- | Inits a mock server with a given scotty monad
mockServer :: ScottyM () -> IO ()
mockServer s = do
  app  <- scottyApp s
  port <- getPort
  run port app

-- | Gets mock server port from env
getPort :: IO Int
getPort = lookupEnv "MOCK_SERVER_PORT" >>= return . read . fromMaybe "4020"

-- Hooks
----------------------------------------------------------------------

-- | Runs spec with a mock server
withMockServer :: ScottyM () -> SpecWith a -> SpecWith a
withMockServer = before_ . void . forkIO . mockServer

-- Req helpers
--------------------------------------------------------------------------------

-- | Runs a request on the mock server
mockReq :: Req a -> IO a
mockReq = (mockHttpConfig >>=) . (flip runReq)

-- | HttpConfig for rewriting requests to point to the mock server
--------------------------------------------------------------------------------
mockHttpConfig :: IO HttpConfig
mockHttpConfig = do
  man <- newManager defaultManagerSettings { managerModifyRequest = modifyReq }
  return defaultHttpConfig { httpConfigAltManager = Just man }

modifyReq req = do
  port <- getPort
  return req { secure = False, host = "localhost", port }
