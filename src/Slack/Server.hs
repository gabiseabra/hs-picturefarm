module Slack.Server
  ( main
  , application
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

main :: IO ()
main = do
  ctx@(_, config, _) <- initializeWithDefaults def
  putStrLn ("Listening on port " <> show (port config))
  run (port config) $ application ctx

application :: AppContext -> Application
application = slashSimple . (. flip (runCmd cmd)) . (>>=) . getEnv

cmd = Emo.cmd
