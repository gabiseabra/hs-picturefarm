module Slack.Auth
  ( application
  )
where

import           Env
import           Slack.Command                  ( runCmd )
import qualified Slack.Command.Emo             as Emo

import           Data.String.Conversions

import           Data.Text.Lazy                 ( Text )

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
                                                , get
                                                , notFound
                                                )

-- Routes
----------------------------------------------------------------------

application :: AppContext -> IO Application
application ctx@(_, config, _) = scottyApp $ router config

router :: Config -> ScottyM ()
router config = do
  get "/new"
    $  status status302
    >> addHeader "Location" (cs $ authURI config)
    >> text "302 Found"

  get "/" $ status status200 >> text "OK"

  notFound $ status status404 >> text "Not found"


--------------------------------------------------------------------------------

authURI :: Config -> String
authURI Config {..} =
  "https://slack.com/oauth/authorize/?client_id="
    <> slackClientId
    <> "&scope="
    <> slackScopes
