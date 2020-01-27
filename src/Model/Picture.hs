module Model.Picture (
  Picture,
  getByUuid,
  findByTags
) where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.UUID

import Database.PostgreSQL.Simple.Types (PGArray)
import qualified Database.PostgreSQL.Simple              as TQ
import qualified Database.PostgreSQL.Simple.TypedQuery   as TQ
import Data.String.QM

import Model (parseOne, parseMany)

data Picture = Picture {
  uuid     :: UUID,
  fileName :: Text,
  fileHash :: Text,
  url      :: Text,
  mimeType :: Text
} deriving (Show)

instance FromJSON Picture where
  parseJSON (Object v) = do
    uuid      <-  v .: "uuid"
    fileName  <-  v .: "file_name"
    fileHash  <-  v .: "file_hash"
    url       <-  v .: "url"
    mimeType  <-  v .: "mime_type"
    return (Picture uuid fileName fileHash url mimeType)

  parseJSON _ = empty

-- Queries
----------------------------------------------------------------------

getByUuid :: Text -> TQ.Connection -> IO (Maybe (Either String Picture))
getByUuid uuid conn = do
  $(TQ.genJsonQuery [qq|
    select uuid      as uuid                     -- UUID
         , file_name as file_name                -- Text
         , url       as url                      -- Text
         , mime_type as mime_type                -- Text
         , file_hash as file_hash                -- Text
    from pictures
    where uuid = ?                               -- < uuid
  |]) conn >>= parseOne

findByTags :: [Text] -> TQ.Connection -> IO (Either String [Picture])
findByTags tags conn = do
  $(TQ.genJsonQuery [qq|
    select p.uuid      as uuid                   -- UUID
         , p.file_name as file_name              -- Text
         , p.url       as url                    -- Text
         , p.mime_type as mime_type              -- Text
         , p.file_hash as file_hash              -- Text
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
  |]) conn >>= parseMany
