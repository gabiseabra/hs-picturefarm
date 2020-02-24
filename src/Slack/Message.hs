{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Slack.Message
  ( MessageBlock(..)
  , SlackMessage(..)
  , ResponseType(..)
  , send
  )
where

import           GHC.Generics

import           Data.Aeson

import           Data.String.Conversions
import qualified Data.Text                     as T
import           Text.URI                       ( mkURI )

import           Control.Exception              ( Exception
                                                , throw
                                                )

import           Network.HTTP.Req               ( Url(..)
                                                , Scheme(..)
                                                , Option(..)
                                                , Req(..)
                                                , ReqBodyJson(..)
                                                , POST(..)
                                                , IgnoreResponse(..)
                                                , req
                                                , runReq
                                                , useHttpsURI
                                                , defaultHttpConfig
                                                , ignoreResponse
                                                )

data InvalidHookUrl = InvalidHookUrl T.Text deriving (Show, Exception)

data MessageBlock = ImageBlock { url :: T.Text, title :: T.Text }

instance ToJSON MessageBlock where
  toJSON ImageBlock {..} = object
    [ "type" .= ("image" :: T.Text)
    , "image_url" .= url
    , "alt_text" .= title
    , "title" .= object
      ["type" .= ("plain_text" :: T.Text), "text" .= title, "emoji" .= True]
    ]

data ResponseType =
    Ephemeral -- Only visible to the user who interacted with the slackbot
  | InChannel -- Visible to all in a channel
  deriving (Show)

instance ToJSON ResponseType where
  toJSON Ephemeral = String "ephemeral"
  toJSON InChannel = String "in_channel"

data SlackMessage = SlackMessage
  { response_type :: ResponseType
  , text :: T.Text
  , blocks :: Maybe [MessageBlock]
  } deriving (Generic, ToJSON)

--------------------------------------------------------------------------------

-- | Send a message to a slack webhook's response_url
--------------------------------------------------------------------------------
send :: SlackMessage -> T.Text -> IO ()
send msg url = do
  _ <- runReq defaultHttpConfig . slackReq msg =<< parseUrl url
  return ()

slackReq
  :: SlackMessage -> (Url 'Https, Option 'Https) -> Req (IgnoreResponse)
slackReq msg (url, opt) = req POST url (ReqBodyJson msg) ignoreResponse opt

parseUrl :: T.Text -> IO (Url 'Https, Option 'Https)
parseUrl uri =
  return . maybe (throw $ InvalidHookUrl uri) (id) . useHttpsURI =<< mkURI uri
