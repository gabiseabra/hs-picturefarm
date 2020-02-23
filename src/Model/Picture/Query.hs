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
       , p.resource_type
       , p.mime_type
       , array_remove(array_agg(pt.tag), NULL)
  from pictures p
  left join picture_tags pt on pt.picture_uuid = p.uuid
  |]

-- Filters
----------------------------------------------------------------------

tagsCTE = Clause
  CTE
  [qq|
  recursive tags_matched (tag) as (
      /* -- Select a list of aliases corresponding to all queried tags */
      select v.value
      from tag_aliases ta
      cross join lateral (
        values ('alias', ta.alias), ('tag', ta.tag)
      ) v (col, value)
      where array[ta.tag,ta.alias]::text[]  && ?tags
    union all
      select ta.alias
      from tag_aliases ta, tags_matched tm
      where ta.tag = tm.tag
  )
|]

tagsFilter = Clause
  JOIN
  [qq|
  inner join picture_tags ptf
    on ptf.picture_uuid = p.uuid
    and (
         ptf.tag = any (?tags)
      or ptf.tag in ( select tag from tags_matched )
    )
  |]

resourceTypeFilter = Clause WHERE [qq| resource_type = ?resourceType|]

-- Queries
----------------------------------------------------------------------

insertPictureQuery :: Query
insertPictureQuery = cs $ [qm|
    insert into pictures (file_name, file_hash, url, resource_type, mime_type)
    values (?, ?, ?, ?, ?)
    returning id, uuid
  |]

updatePictureQuery :: Query
updatePictureQuery = cs $ [qm|
    update pictures
    set file_name     = ?
      , file_hash     = ?
      , url           = ?
      , resource_type = ?
      , mime_type     = ?
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
  let cteClause   = buildClause CTE filters
      joinClause  = buildClause JOIN filters
      whereClause = buildClause WHERE filters
  in  cs $ [qm|
    ${cteClause}
    ${fromPictures}
    ${joinClause}
    ${whereClause}
    group by p.id
    order by ?orderBy
    limit ?limit offset ?offset
  |]
