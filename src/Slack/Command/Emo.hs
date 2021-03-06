module Slack.Command.Emo
  ( cmd
  )
where

import           Env
import           Service.Cloudinary             ( CloudinaryResource(..) )
import           Slack.Message                  ( SlackMessage(..)
                                                , ResponseType(..)
                                                , MessageBlock(..)
                                                , BlockElement(..)
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

-- | Parser of the `/emo` command
--------------------------------------------------------------------------------

cmd :: CommandParser
cmd _ (Command "emo" _ _ (Just "help") _ _) = return $ Just $ SlackMessage
  { response_type = Ephemeral
  , text          = ""
  , blocks        =
    Just
      [ Section
      $  Markdown
      $  "Use `/emo` to get a random picture of an animal, "
      <> "optionally pass an emoji to get more specific results. "
      <> "Try `/emo :scream:`"
      , Context
        [ Markdown
          $  "🐞 Got a bug? Picturefarm is open source, "
          <> "report an issue on "
          <> "<https://github.com/gabiseabra/hs-picturefarm/issues|Github>"
        ]
      ]
  }

cmd Env { config, conn } (Command "emo" user chan message _ _) = do
  pic <- findOnePicture conn message
  case pic of
    Nothing -> return $ Just $ SlackMessage
      { response_type = Ephemeral
      , text          = "404"
      , blocks        = Just [Image notFoundUrl (formatTitle message Nothing)]
      }
    Just pic -> return $ Just $ SlackMessage
      { response_type = InChannel
      , text          = fileName pic
      , blocks        =
        Just [Image (cdnPublicUrl pic config) (formatTitle message (Just pic))]
      }

cmd _ _ = return Nothing

--------------------------------------------------------------------------------

findOnePicture :: Connection -> Maybe T.Text -> IO (Maybe Picture)
findOnePicture conn message =
  let
    tags       = parseTags message
    pagination = Just PaginationInput { page = Just 1, pageSize = Just 1 }
    input =
      def { tags
          , resourceType = Just "image"
          , orderBy      = Pic.Random
          , pagination
          } :: FindPicturesInput
  in
    fmap headZ $ catch (Pic.findPictures conn input) $ \(_ :: SomeException) ->
      return []

notFoundUrl = "https://http.cat/404"

parseTags Nothing    = Nothing
parseTags (Just "" ) = Nothing
parseTags (Just tag) = Just [tag]

formatTitle :: Maybe T.Text -> Maybe Picture -> T.Text
formatTitle (Just msg) (Just pic) = msg <> " - " <> fileName pic
formatTitle Nothing    (Just pic) = fileName pic
formatTitle _          Nothing    = "got nothing"
