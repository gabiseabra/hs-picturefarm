module Spec.WebCase
  ( module Spec.TestCase
  , module Test.Hspec.Wai
  , Application
  , qq
  , json
  , postGQL
  , setupApplication
  , withApplication
  , aroundApplication
  , bodyContains
  )
where

import           Env
import           Web.Router                     ( application )
import           Spec.TestCase

import           Test.Hspec.Wai                 ( WaiSession
                                                , withState
                                                , post
                                                , matchBody
                                                , shouldRespondWith
                                                )
import           Test.Hspec.Wai.Matcher         ( Body
                                                , MatchBody(..)
                                                )
import           Test.Hspec.Wai.JSON            ( json )

import           Data.String.QM                 ( qq
                                                , qm
                                                )
import           Data.ByteString                ( isInfixOf )
import           Data.ByteString.Lazy           ( ByteString
                                                , toStrict
                                                )
import           Data.String.Conversions        ( cs )
import           Data.Default.Class

import           Control.Concurrent.Async       ( concurrently )
import           Control.Monad.IO.Class         ( liftIO )

import           Network.Wai                    ( Application )
import           Network.Wai.Test               ( SResponse )

-- Hooks
----------------------------------------------------------------------

-- | Runs a spec with application state for hspec-wai
withApplication :: IO st -> SpecWith (st, Application) -> Spec
withApplication = withState . statefulApplication

-- | Hspec's around hook with additional application state for hspec-wai
aroundApplication
  :: (ActionWith st -> IO ()) -> SpecWith (st, Application) -> Spec
aroundApplication action = around $ \inner -> do
  action $ \st -> (statefulApplication $ pure st) >>= inner

setupContext :: IO AppContext
setupContext = do
  config <- loadConfigWithDefaults (Just def) (Just Test)
  pool   <- createConnectionPool config
  return (Just Test, config, pool)

setupApplication :: IO Application
setupApplication = setupContext >>= application

statefulApplication :: IO st -> IO (st, Application)
statefulApplication = flip concurrently $ setupApplication

----------------------------------------------------------------------

-- | Posts a request to the GraphQL endpoint
postGQL :: ByteString -> ByteString -> WaiSession st SResponse
postGQL q v = post "/api" $ buildGraphQLRequest q v

buildGraphQLRequest :: ByteString -> ByteString -> ByteString
buildGraphQLRequest query variables =
  let variables' = cs variables
      query'     = cs query
  in  cs $ [qm|{"variables": ${variables'},"query": "${query'}"}|]

-- Expectations
----------------------------------------------------------------------

-- | Check whether a response body contains a given substring.
-- Gotten from https://github.com/lpil/captain/blob/3e2307/test/Support.hs
bodyContains :: Body -> MatchBody
bodyContains subString = MatchBody bodyContains'
 where
  bodyContains' _ body = if toStrict subString `isInfixOf` toStrict body
    then Nothing
    else Just $ errorMsg body
  errorMsg body = unlines
    ["Body did not contain `" ++ show subString ++ "`", "Body:", show body]
