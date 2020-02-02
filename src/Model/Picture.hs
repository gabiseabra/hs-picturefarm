module Model.Picture
  ( RecordError(..)
  , Picture(..)
  , getByUuid
  , findByTags )
where

import           Model

import           Control.Monad
import           Control.Applicative

import           Data.Aeson
import           Data.Maybe
import           Data.Text (Text)
import           Data.UUID
import           Data.String.QM

import qualified Database.PostgreSQL.Simple            as PG
import qualified Database.PostgreSQL.Simple.TypedQuery as TQ

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

getByUuid :: UUID -> PG.Connection -> IO (Either RecordError Picture)
getByUuid uuid conn = do
  $(TQ.genJsonQuery [qq|
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

findByTags :: [Text] -> PG.Connection -> IO (Either RecordError [Picture])
findByTags tags conn = do
  $(TQ.genJsonQuery [qq|
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
  |]) conn >>= parseMany
