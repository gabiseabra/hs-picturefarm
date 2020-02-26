module Slack.Auth
  ( application
  )
where

import           Env
import           Slack.Command                  ( runCmd )
import qualified Slack.Command.Emo             as Emo

import           Data.String.Conversions
import           Data.Text.Lazy                 ( Text )

import           Control.Monad                  ( void )

import           Network.Wai                    ( Application )
import           Network.HTTP.Types             ( status200
                                                , status302
                                                , status404
                                                )
import           Web.Scotty                     ( ScottyM
                                                , scottyApp
                                                , addHeader
                                                , status
                                                , text
                                                , param
                                                , get
                                                , notFound
                                                , liftAndCatchIO
                                                )
import           Network.HTTP.Req               ( Req
                                                , POST(..)
                                                , ReqBodyUrlEnc(..)
                                                , (/:)
                                                , (=:)
                                                , https
                                                , header
                                                , req
                                                , runReq
                                                , useHttpsURI
                                                , defaultHttpConfig
                                                , bsResponse
                                                , responseBody
                                                )
-- Routes
--------------------------------------------------------------------------------

application :: AppContext -> IO Application
application ctx@(_, config, _) = scottyApp $ router config

router :: Config -> ScottyM ()
router config = do
  get "/new"
    $  addHeader "Location" (cs $ authURI config)
    >> status status302
    >> text "Found"

  get "/grant"
    $   param "code"
    >>= liftAndCatchIO
    .   authorize config
    >>  status status200
    >>  text "OK"

  notFound $ status status404 >> text "Not found"

--------------------------------------------------------------------------------

-- | Get redirect uri for slack to request permission to access a workspace.
authURI :: Config -> String
authURI Config {..} =
  "https://slack.com/oauth/authorize/?client_id="
    <> slackClientId
    <> "&scope="
    <> slackScopes

-- | Send request to https://slack.com/api/oauth.v2.access after permission is
--   granted on a workspace. It just discards the access token because it isn't
--   really needed for slash commands.
authorize :: Config -> Text -> IO ()
authorize Config {..} code =
  let url'     = https "slack.com" /: "api" /: "oauth.v2.access"
      headers' = header "Content-Type"
                        "application/x-www-form-urlencoded; charset=UTF-8"
      params' =
          ("client_id" =: slackClientId)
            <> ("client_secret" =: slackClientSecret)
            <> ("code" =: code)
      req' = req POST url' (ReqBodyUrlEnc params') bsResponse headers'
  in  runReq defaultHttpConfig req' >>= putStrLn . cs . responseBody

