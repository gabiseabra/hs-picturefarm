module Model.Picture.Query where

import           Database.QueryBuilder

import           Data.List                      ( concat
                                                , intersperse
                                                )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Data.ByteString.Builder        ( string8 )
import           Data.String.Conversions        ( cs )
import           Data.String.QM

import           Database.PostgreSQL.Simple     ( Query )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(..)
                                                , ToField(..)
                                                )

type PictureFields = (Int, UUID, Text, Text, Text, Text, [Text])

data IndexedField = ID | UUID | FileName | FileHash

instance Show IndexedField where
  show ID       = "id"
  show UUID     = "uuid"
  show FileName = "file_name"
  show FileHash = "file_hash"

instance ToField IndexedField where
  toField = Plain . string8 . show

fromPictures = [qq|
  select p.id
       , p.uuid
       , p.file_name
       , p.file_hash
       , p.url
       , p.mime_type
       , array_agg(pt.tag)
  from pictures p
  inner join picture_tags pt on pt.picture_uuid = p.uuid
  |]

-- Filters
----------------------------------------------------------------------

tagsFilter = Filter
  JOIN
  [qq|
  inner join picture_tags ptw
    on ptw.picture_uuid = p.uuid
    and ptw.tag in (
      /* -- Select a list of aliases corresponding to all queried tags */
      select v.value
      from tag_aliases ta
      cross join lateral (
        values ('alias', ta.alias), ('tag', ta.tag)
      ) v (col, value)
      where array[ta.tag,ta.alias]::text[]  && ?tags
      union all
      select unnest( ?tags )
    )
  |]

-- Queries
----------------------------------------------------------------------

insertPictureQuery :: Query
insertPictureQuery = cs $ [qm|
    insert into pictures (file_name, file_hash, url, mime_type)
    values (?, ?, ?, ?)
    returning id, uuid
  |]

updatePictureQuery :: Query
updatePictureQuery = cs $ [qm|
    update pictures
    set file_name = ?
      , file_hash = ?
      , url       = ?
      , mime_type = ?
    where uuid = ?
  |]

deletePictureTagsQuery :: Query
deletePictureTagsQuery = cs $ [qm|
  delete from picture_tags where picture_uuid = ?
|]

insertPictureTagQuery :: Query
insertPictureTagQuery = cs $ [qm|
    insert into picture_tags (picture_uuid, tag)
    values (?, ?)
  |]

getPictureByQuery :: Query
getPictureByQuery = cs $ [qm|
    ${fromPictures}
    where p.?field = ?value
    group by p.id
    limit 1
  |]

findPictureQuery :: (QueryOptions a) => a -> Query
findPictureQuery filters =
  let joinClause  = buildClause JOIN filters
      whereClause = buildClause WHERE filters
  in  cs $ [qm|
    ${fromPictures}
    ${joinClause}
    ${whereClause}
    group by p.id
    order by ?orderBy
    limit ?limit offset ?offset
  |]
