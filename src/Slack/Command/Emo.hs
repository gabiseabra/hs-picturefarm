module Slack.Command.Emo
  ( cmd
  )
where

import           Env
import           Slack.Message                  ( SlackMessage(..)
                                                , MessageBlock(..)
                                                )
import           Slack.Command

import           Env
import           Model.Picture                  ( Picture(..)
                                                , FindPicturesInput(..)
                                                )
import qualified Model.Picture                 as Pic
import           Model.Pagination               ( PaginationInput(..) )

import qualified Data.Text                     as T
import           Data.Default.Class
import           Data.String.Conversions

import           Control.Error.Safe             ( headZ )
import           Control.Exception              ( SomeException
                                                , throw
                                                , catch
                                                )

import           Network.Linklater              ( Command(..) )

publicUrl :: Config -> T.Text -> T.Text
publicUrl Config { cdnPublicUrl } url = (cs cdnPublicUrl) <> "/" <> url

cmd :: CommandParser
cmd Env { config, conn } (Command "emo" user chan message _ _) = do
  pic <- findOnePicture conn message
  case pic of
    Nothing -> return $ Just $ SlackMessage
      "got nothing"
      (Just [ImageBlock notFoundUrl "404"])
    Just Picture { fileName, url } -> return $ Just $ SlackMessage
      fileName
      (Just [ImageBlock (publicUrl config url) fileName])
cmd _ _ = return Nothing

findOnePicture :: Connection -> Maybe T.Text -> IO (Maybe Picture)
findOnePicture conn message =
  let
    tags = parseTags message
    pagination = Just PaginationInput { page = Just 1, pageSize = Just 1 }
    input = def { tags, orderBy = Pic.Random, pagination } :: FindPicturesInput
  in
    fmap headZ $ catch (Pic.findPictures conn input) $ \(_ :: SomeException) ->
      return []

notFoundUrl = "https://http.cat/404"

parseTags Nothing    = Nothing
parseTags (Just tag) = Just [tag]
