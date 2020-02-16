{-# LANGUAGE ScopedTypeVariables #-}

module Spec.ModelCase
  ( module Spec.ConnCase
  , PictureInput
  , PictureTagInput
  , TagAliasInput
  , insertPictures
  , insertPictureTags
  , insertTagAliases
  , cleanupDB
  )
where

import           Spec.ConnCase

import           Model.Picture

import           Data.UUID
import           Data.Text                      ( Text )

import           Database.PostgreSQL.Simple     ( Connection
                                                , returning
                                                , fromOnly
                                                , executeMany
                                                , execute_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ

type PictureInput = (Text, Text, Text, Text)

type PictureTagInput = (UUID, [Text])

type TagAliasInput = (Text, Text)

-- Test helpers
----------------------------------------------------------------------

-- | Inserts a list of pictures and returns their respective uuids
insertPictures :: Connection -> [PictureInput] -> IO [UUID]
insertPictures conn input =
  map fromOnly <$> returning conn insertPictureQuery input

-- | Inserts a list of tags to pictures
insertPictureTags :: Connection -> [PictureTagInput] -> IO ()
insertPictureTags conn input =
  executeMany conn
              insertPictureTagQuery
              (concat . map unnestPictureTagInput $ input)
    >> return ()

unnestPictureTagInput :: PictureTagInput -> [(UUID, Text)]
unnestPictureTagInput (uuid, tags) =
  flip zip tags $ take (length tags) $ repeat uuid

-- | Inserts a list of tag aliases
insertTagAliases :: Connection -> [TagAliasInput] -> IO ()
insertTagAliases conn input =
  executeMany conn insertTagAliasQuery input >> return ()

-- | Removes all records from the database
cleanupDB :: Connection -> IO Connection
cleanupDB conn = do
  _ <- execute_
    conn
    [sql|
    set client_min_messages to warning;
    truncate tag_aliases cascade;
    truncate picture_tags cascade;
    truncate pictures cascade;
    |]
  return conn

----------------------------------------------------------------------

insertPictureQuery = [sql|
  insert into pictures (url, file_name, file_hash, mime_type)
  values (?, ?, ?, ?)
  returning uuid
  |]

insertPictureTagQuery = [sql|
  insert into picture_tags (picture_uuid, tag)
  values (?, ?)
  |]

insertTagAliasQuery = [sql|
  insert into tag_aliases (tag, alias)
  values (?, ?)
  |]
