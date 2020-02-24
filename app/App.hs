module App where

import           Env
import           Web.Middleware.Router
import qualified Web.Server                    as Web
import qualified Slack.Server                  as Slack

import           Network.Wai                    ( Application
                                                , Middleware
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout
                                                , logStdoutDev
                                                )
import           Network.Wai.Handler.Warp

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
  return $ middleware ctx $ router [match "/slack" slackApp] webApp

middleware :: AppContext -> Middleware
middleware (Just Production, _, _) = logStdout
middleware _                       = logStdoutDev

onException _req e = print ("Error: " ++ show e)

warpSettings :: Config -> Settings
warpSettings config@Config { port } =
  setPort port $ setOnException onException defaultSettings
