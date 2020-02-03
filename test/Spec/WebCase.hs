module Spec.WebCase
  ( module Spec.TestCase
  , module Test.Hspec.Wai
  , Application
  , qq
  , json
  , postGQL
  , setupApplication
  , withApplication
  -- , aroundApplication
  )
where

import           Env
import           Env.Connection
import           Web.Router                     ( application )
import           Spec.TestCase

import           Test.Hspec.Wai          hiding ( pending
                                                , pendingWith
                                                , withApplication
                                                )
import           Test.Hspec.Wai.JSON            ( json )

import           Data.String.QM                 ( qq
                                                , qm
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.String.Conversions        ( cs )

import           Control.Concurrent.Async       ( concurrently )

import           Network.Wai                    ( Application )
import           Network.Wai.Test               ( SResponse )

setupContext :: IO AppContext
setupContext = do
  config <- loadConfig (Just Test)
  pool   <- createConnectionPool config
  return (Just Test, config, pool)

setupApplication :: IO Application
setupApplication = setupContext >>= application

statefulApplication = flip concurrently $ setupApplication

withApplication :: IO st -> SpecWith (st, Application) -> Spec
withApplication = withState . statefulApplication
{-
aroundApplication
  :: (ActionWith st -> IO ()) -> SpecWith (st, Application) -> Spec
aroundApplication action = around $ \a -> action $ \st -> a $ st
-}
buildGraphQLRequest :: ByteString -> ByteString -> ByteString
buildGraphQLRequest query variables =
  let variables' = cs variables
      query'     = cs query
  in  cs $ [qm|{"variables": ${variables'},"query": "${query'}"}|]

postGQL :: ByteString -> ByteString -> WaiSession st SResponse
postGQL q v = post "/api" $ buildGraphQLRequest q v
