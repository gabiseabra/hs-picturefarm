module Model.Picture
  ( RecordError(..)
  , Picture(..)
  , OrderBy(..)
  , getByUuid
  , findByTags )
where

import           Model
import           Model.Pagination

import           Control.Monad
import           Control.Applicative (empty)

import           Data.Aeson
import           Data.Maybe
import           Data.Text (Text)
import           Data.UUID
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

-- Queries
----------------------------------------------------------------------

data OrderBy = UpdatedAt | Random

instance ToField OrderBy where
  toField UpdatedAt = Plain $ string8 "updated_at"
  toField Random    = Plain $ string8 "random()"

getByUuid :: UUID -> PG.Connection -> IO (Either RecordError Picture)
getByUuid uuid conn = do
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
    where p.uuid = ?                             -- < uuid
    group by p.uuid
  |]) conn >>= parseOne

findByTags :: ([Text], Maybe PaginationInput, OrderBy) -> PG.Connection -> IO (Either RecordError [Picture])
findByTags (tags, pgn, orderBy) conn =
  let PaginationParams {..} = parsePaginationInput pgn
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
      desc
    limit ?                                      -- < limit
    offset ?                                     -- < offset
  |]) conn >>= parseMany
