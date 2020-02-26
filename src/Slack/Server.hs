module Slack.Server
  ( application
  )
where

import           Env
import           Web.Middleware.Router
import           Slack.Command                  ( runCmd )
import qualified Slack.Command.Emo             as Emo
import qualified Slack.Auth                    as Auth

import qualified Data.ByteString.Char8         as BS

import           Network.Linklater              ( slashSimple )

import           Network.Wai.Middleware.SlackVerify
                                                ( verifySlackRequest )
import           Network.Wai                    ( Application )

import           Control.Monad
import           Control.Exception              ( Exception )

data InvalidOAuthURI = InvalidOAuthURI deriving (Show, Exception)

application :: AppContext -> IO Application
application ctx = do
  authApp <- Auth.application ctx
  return $ router [match "/auth" authApp] (slashApp ctx)

slashApp ctx@(_, Config {..}, _) =
  verifySlackRequest (BS.pack slackSigningSecret) $ slashApp' ctx

slashApp' :: AppContext -> Application
slashApp' = slashSimple . (. flip (runCmd cmd)) . withEnv

cmd = Emo.cmd
