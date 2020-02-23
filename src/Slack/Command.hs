{-# LANGUAGE FlexibleContexts #-}

module Slack.Command
  ( CommandParser(..)
  , runCmd
  )
where

import           Env                            ( Env(..)
                                                , Config(..)
                                                )
import           Slack.Message                  ( SlackMessage
                                                , send
                                                )

import           Data.Aeson                     ( encode )
import           Data.String.Conversions
import qualified Data.Text                     as T

import           Network.Linklater              ( Command(..) )

type CommandParser = (Env -> Command -> IO (Maybe SlackMessage))

respUrl :: Command -> T.Text
respUrl (Command _ _ _ _ url _) = url

runCmd :: CommandParser -> Env -> Command -> IO T.Text
runCmd cmd env@Env { config } c = do
  _ <- maybe (return ()) (flip send $ respUrl c) =<< cmd env c
  return ""
