module Model.Picture.Query where

import           Database.QueryBuilder

import           Data.List                      ( concat
                                                , intersperse
                                                )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Data.String.Conversions        ( cs )
import           Data.String.QM

import           Database.PostgreSQL.Simple     ( Query )

type PictureFields = (Int, UUID, Text, Text, Text, Text, [Text])

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

getPictureByQuery :: Query
getPictureByQuery = cs $ [qm|
    ${fromPictures}
    where ?field = ?value
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
