module Slack.Server
  ( application
  )
where

import           Env
import           Slack.Command                  ( runCmd )
import qualified Slack.Command.Emo             as Emo

import           Data.Default.Class

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( (<=<) )
import           Control.Exception              ( SomeException
                                                , Exception
                                                , catch
                                                , throw
                                                )

import           Network.Linklater
import           Network.Wai.Handler.Warp
import           Network.Wai                    ( Application )

import           Control.Monad

application :: AppContext -> IO Application
application = return . slashSimple . (. flip (runCmd cmd)) . (>>=) . getEnv

cmd = Emo.cmd
