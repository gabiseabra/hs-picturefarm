module Spec.WebCase
  ( module Spec.TestCase
  , module Test.Hspec.Wai
  , Application
  , qq
  , json
  , postGQL
  , withApplication
  , setupApplication
  )
where

import           Env
import           Env.Connection
import           Web.Router                     ( application )
import           Spec.TestCase

import           Test.Hspec.Wai          hiding ( pending
                                                , pendingWith
                                                )
import           Test.Hspec.Wai.JSON            ( json )

import           Data.String.QM                 ( qq
                                                , qm
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.String.Conversions        ( cs )

import           Network.Wai                    ( Application )
import           Network.Wai.Test               ( SResponse )

setupContext :: IO AppContext
setupContext = do
  config <- loadConfig (Just Test)
  pool   <- createConnectionPool config
  return (Just Test, config, pool)

setupApplication :: IO Application
setupApplication = setupContext >>= application

withApplication :: SpecWith Application -> Spec
withApplication = with setupApplication

buildGraphQLRequest :: ByteString -> ByteString -> ByteString
buildGraphQLRequest query variables =
  let variables' = cs variables
      query'     = cs query
  in  cs $ [qm|{"variables": ${variables'},"query": "${query'}"}|]

postGQL :: ByteString -> ByteString -> WaiSession SResponse
postGQL q v = post "/api" $ buildGraphQLRequest q v
