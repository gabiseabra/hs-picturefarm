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
                                                , Command(..)
                                                , say
                                                )
import           Network.Linklater.Types        ( RequestError )
import qualified Network.Linklater             as LL

data CommandError = InvalidCommand | CommandError String deriving (Eq, Show, Exception)

type CommandParser = (Env -> Command -> IO (Maybe Message))

cmdConfig :: Command -> LL.Config
cmdConfig (Command _ _ _ _ url _) = LL.Config url

runCmd :: CommandParser -> Env -> Command -> IO T.Text
runCmd cmd env@Env { config } c = do
  res <- maybe (return $ Right ()) (runExceptT . sayMsg c) =<< cmd env c
  case res of
    Right _   -> return ""
    Left  err -> throw $ CommandError $ show err

sayMsg :: (MonadError RequestError m, MonadIO m) => Command -> Message -> m ()
sayMsg = (flip say) . cmdConfig

fmtMsg = FormattedMessage (EmojiIcon "horse") "picturefarm"
