{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Slack.Message
  ( MessageBlock(..)
  , SlackMessage(..)
  , send
  )
where

import           GHC.Generics

import           Data.Aeson
-- import           Data.String.Conversions
import qualified Data.Text                     as T
import           Text.URI                       ( URI
                                                , mkURI
                                                )

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( (<=<) )
import           Data.String.Conversions

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

data InvalidSlackHookUrl = InvalidSlackHookUrl T.Text deriving (Show, Exception)

data MessageBlock = ImageBlock { url :: T.Text, title :: T.Text }

instance ToJSON MessageBlock where
  toJSON ImageBlock {..} = object
    [ "type" .= ("image" :: T.Text)
    , "image_url" .= url
    , "alt_text" .= title
    , "title" .= object
      ["type" .= ("plain_text" :: T.Text), "text" .= title, "emoji" .= True]
    ]

data SlackMessage = SlackMessage
  { text :: T.Text
  , blocks :: Maybe [MessageBlock]
  } deriving (Generic, ToJSON)

send :: SlackMessage -> T.Text -> IO ()
send msg url = do
  _ <- runReq defaultHttpConfig . slackReq msg =<< parseUrl url
  return ()

slackReq
  :: SlackMessage -> (Url 'Https, Option 'Https) -> Req (IgnoreResponse)
slackReq msg (url, opt) = req POST url (ReqBodyJson msg) ignoreResponse opt

parseUrl :: T.Text -> IO (Url 'Https, Option 'Https)
parseUrl uri =
  return
    .   maybe (throw $ InvalidSlackHookUrl uri) (id)
    .   useHttpsURI
    <=< mkURI
    $   uri
