{-# LANGUAGE ScopedTypeVariables #-}

module Spec.ModelCase
  ( module Spec.ConnCase
  , PictureInput
  , PictureTagInput
  , TagAliasInput
  , insertPictures
  , insertPictureTags
  , insertTagAliases
  , cleanUpDB
  )
where

import           Spec.ConnCase

import           Model.Picture

import           Data.UUID
import           Data.Text                      ( Text )

type PictureInput = (Text, Text, Text, Text)

type PictureTagInput = (UUID, [Text])

type TagAliasInput = (Text, Text)

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

insertPictures :: Connection -> [PictureInput] -> IO [UUID]
insertPictures conn input =
  map fromOnly <$> returning conn insertPictureQuery input

unnestPictureTagInput :: PictureTagInput -> [(UUID, Text)]
unnestPictureTagInput (uuid, tags) =
  flip zip tags $ take (length tags) $ repeat uuid

insertPictureTags :: Connection -> [PictureTagInput] -> IO ()
insertPictureTags conn input =
  executeMany conn
              insertPictureTagQuery
              (concat . map unnestPictureTagInput $ input)
    >> return ()

insertTagAliases :: Connection -> [TagAliasInput] -> IO ()
insertTagAliases conn input =
  executeMany conn insertTagAliasQuery input >> return ()

cleanUpDB :: Connection -> IO Connection
cleanUpDB conn = do
  _ <- execute_
    conn
    [sql|
    set client_min_messages to warning;
    truncate tag_aliases cascade;
    truncate picture_tags cascade;
    truncate pictures cascade;
    |]
  return conn

