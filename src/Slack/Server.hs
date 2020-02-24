module Slack.Server
  ( application
  )
where

import           Env
import           Web.Middleware.Router
import           Slack.Command                  ( runCmd )
import qualified Slack.Command.Emo             as Emo

import           Data.String.Conversions
import qualified Data.ByteString.Char8         as BS

import           Network.Linklater              ( slashSimple )
import           Network.Wai
import           Network.HTTP.Types

import           Network.Wai.Middleware.SlackVerify
                                                ( verifySlackRequest )
import           Network.Wai                    ( Application )

import           Control.Monad
import           Control.Exception              ( Exception
                                                , throw
                                                )

data InvalidOAuthURI = InvalidOAuthURI deriving (Show, Exception)

application :: AppContext -> IO Application
application ctx = return $ router [dir "/auth" (authApp ctx)] (slashApp ctx)

slashApp :: AppContext -> Application
slashApp = slashSimple . (. flip (runCmd cmd)) . (>>=) . getEnv

authApp :: AppContext -> Application
authApp (_, config, _) _ send =
  send $ responseLBS found302 [("Location", cs $ authURI config)] "302 Found"

authURI :: Config -> String
authURI Config {..} =
  "https://slack.com/oauth/authorize/?client_id="
    <> slackClientId
    <> "&scope="
    <> slackScopes

cmd = Emo.cmd
