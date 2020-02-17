{-# LANGUAGE FlexibleContexts #-}

module Slack.Command
  ( CommandParser(..)
  , CommandError(..)
  , runCmd
  , sayMsg
  , fmtMsg
  )
where

import           Env                            ( Env(..)
                                                , Config(..)
                                                )

import           Data.String.Conversions
import qualified Data.Text                     as T

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad                  ( (<=<) )
import           Control.Monad.Except           ( MonadError
                                                , runExceptT
                                                )
import           Control.Exception              ( Exception
                                                , throw
                                                )

import           Network.Linklater              ( Message(..)
                                                , Icon(..)
                                                , Command
                                                , say
                                                )
import           Network.Linklater.Types        ( RequestError )
import qualified Network.Linklater             as LL

data CommandError = InvalidCommand | CommandError String deriving (Eq, Show, Exception)

type CommandParser = (Env -> Command -> IO (Maybe Message))

slackConfig :: Config -> LL.Config
slackConfig = LL.Config . cs . slackToken

runCmd :: CommandParser -> Env -> Command -> IO T.Text
runCmd cmd env@Env { config } c = do
  res <- maybe (return $ Right ()) (runExceptT . sayMsg config) =<< cmd env c
  case res of
    Right _   -> return ""
    Left  err -> throw $ CommandError $ show err

sayMsg :: (MonadError RequestError m, MonadIO m) => Config -> Message -> m ()
sayMsg = (flip say) . slackConfig

fmtMsg = FormattedMessage (EmojiIcon "horse") "picturefarm"
