module App where

import           Env
import qualified Web.Server                    as Web
import qualified Slack.Server                  as Slack

import           Network.Wai                    ( Application )
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Router

main :: IO ()
main = do
  ctx@(_, config, _) <- initialize
  app                <- application ctx
  putStrLn ("Starting server on http://localhost:" ++ show (port config))
  runSettings (warpSettings config) app

application :: AppContext -> IO Application
application ctx = do
  webApp   <- Web.application ctx
  slackApp <- Slack.application ctx
  return (router [dir "/slack" slackApp] webApp)

onException _req e = print ("Error: " ++ show e)

warpSettings :: Config -> Settings
warpSettings config@Config { port } =
  setPort port $ setOnException onException defaultSettings
