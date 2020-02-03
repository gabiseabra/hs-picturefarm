module Model.Picture
  ( module Defaults
  , module Model.Pagination
  , RecordError(..)
  , Picture(..)
  , OrderBy(..)
  , Order(..)
  , IndexedField(..)
  , FindByTagsInput(..)
  , getByUUID
  , getByFileName
  , findByTags )
where

import           Defaults
import           Model
import           Model.Pagination

import           Control.Monad
import           Control.Applicative (empty)

import           Data.Aeson
import           Data.Maybe
import           Data.Text (Text)
import           Data.UUID ( UUID )
import           Data.String.QM

import           Data.ByteString.Builder        ( string8 )

import qualified Database.PostgreSQL.Simple    as PG
import Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))
import Database.PostgreSQL.Simple.TypedQuery (genJsonQuery)

-- Schema
----------------------------------------------------------------------

data Picture = Picture {
  uuid     :: UUID,
  fileName :: Text,
  fileHash :: Text,
  url      :: Text,
  mimeType :: Text,
  tags     :: [Text]
} deriving (Show)

instance FromJSON Picture where
  parseJSON (Object v) = do
    uuid      <-  v .: "uuid"
    fileName  <-  v .: "file_name"
    fileHash  <-  v .: "file_hash"
    url       <-  v .: "url"
    mimeType  <-  v .: "mime_type"
    tags      <-  v .: "tags"
    return (Picture uuid fileName fileHash url mimeType tags)

  parseJSON _ = empty

-- Input types
----------------------------------------------------------------------

data Order = ASC | DESC deriving Show

data IndexedField = UUID | FileName | UpdatedAt

instance Show IndexedField where
  show UUID      = "uuid"
  show FileName  = "file_name"
  show UpdatedAt = "updated_at"

instance ToField IndexedField where
  toField = Plain . string8 . show

data OrderBy = OrderBy IndexedField Order | Random

instance ToField OrderBy where
  toField (OrderBy field ord) =
    Plain $ string8 $ show field ++ " " ++ show ord
  toField Random              = Plain $ string8 "random()"

data FindByTagsInput = FindByTagsInput {
  tags :: [Text],
  orderBy :: OrderBy,
  pagination :: Maybe PaginationInput
}

instance Defaults FindByTagsInput where
  def = FindByTagsInput [] (OrderBy UpdatedAt DESC) Nothing

-- Queries
----------------------------------------------------------------------

-- | Returns one picture
getBy :: (ToField a) => IndexedField -> a -> PG.Connection -> IO (Either RecordError Picture)
getBy field value conn = do
  $(genJsonQuery [qq|
    select p.uuid                 as uuid        -- UUID
         , p.file_name            as file_name   -- Text
         , p.url                  as url         -- Text
         , p.mime_type            as mime_type   -- Text
         , p.file_hash            as file_hash   -- Text
         , array_agg(pt.tag)      as tags        -- [Text]
    from pictures p
    left join picture_tags pt
      on pt.picture_uuid = p.uuid
    where p.?                                    -- < field
          = ?                                    -- < value
    group by p.uuid
  |]) conn >>= parseOne

getByUUID :: UUID -> PG.Connection -> IO (Either RecordError Picture)
getByUUID = getBy UUID

getByFileName :: Text -> PG.Connection -> IO (Either RecordError Picture)
getByFileName = getBy FileName

-- | Returns a list of pictures with any of the given tags
findByTags
  :: FindByTagsInput -> PG.Connection -> IO (Either RecordError [Picture])
findByTags FindByTagsInput{..} conn =
  let PaginationParams {..} = parsePaginationInput pagination
  in $(genJsonQuery [qq|
    select p.uuid                 as uuid        -- UUID
         , p.file_name            as file_name   -- Text
         , p.url                  as url         -- Text
         , p.mime_type            as mime_type   -- Text
         , p.file_hash            as file_hash   -- Text
         , array_agg(pt.tag)      as tags        -- [Text]
    from pictures p
    inner join picture_tags pt
      on pt.picture_uuid = p.uuid
    where pt.tag in (
      /* -- Select a list of aliases corresponding to all queried tags */
      select distinct(v.value)
      from tag_aliases ta
      cross join lateral (
        values ('alias', ta.alias), ('tag', ta.tag)
      ) v (col, value)
      where array[ta.tag,ta.alias]::text[]  && ? -- < tags
      union all
      select unnest(
        ?                                        -- < tags
      )
    )
    group by p.uuid
    order by ?                                   -- < orderBy
    limit ?                                      -- < limit
    offset ?                                     -- < offset
  |]) conn >>= parseMany
