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
-- Hooks
----------------------------------------------------------------------

-- | Run block with application state for hspec-wai
-- with :: IO st -> SpecWith (st, Application) -> Spec
-- with = withState . statefulApplication
-- 

_port = 4020

mockServer :: ScottyM () -> IO ()
mockServer s = do
  app <- scottyApp s
  run _port app

withMockServer :: ScottyM () -> SpecWith a -> SpecWith a
withMockServer = before_ . void . forkIO . mockServer

modifyReq :: Request -> IO Request
modifyReq req = return req { secure = False, port = _port, host = "localhost" }

mockHttpConfig = do
  man <- newManager defaultManagerSettings { managerModifyRequest = modifyReq }
  return defaultHttpConfig { httpConfigAltManager = Just man }

mockReq :: Req a -> IO a
mockReq = (mockHttpConfig >>=) . (flip runReq)
