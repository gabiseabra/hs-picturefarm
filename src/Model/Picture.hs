{-# LANGUAGE DeriveAnyClass #-}

module Model.Picture
  ( RecordError(..)
  , Picture(..)
  , OrderBy(..)
  , Order(..)
  , IndexedField(..)
  , FindByTagsInput(..)
  , GetAllInput(..)
  , getByUUID
  , getByFileName
  , getAll
  , findByTags )
where

import GHC.Generics
import           Defaults
import           Model
import           Model.Pagination
import           Model.Picture.Query

import           Control.Monad
import           Control.Monad.Error.Class (liftEither)
import           Control.Applicative (empty)

import           Data.Aeson
import           Data.Maybe
import           Data.Text (Text)
import           Data.UUID ( UUID )
import           Data.String.QM
import           Data.Tuple.Curry (uncurryN)

import           Data.ByteString.Builder        ( string8 )

import            Database.PostgreSQL.Simple    ( Only(..), Connection )
import            Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))
import           Database.PostgreSQL.Simple.FromRow ( FromRow(..) )
import            Database.PostgreSQL.Simple.TypedQuery (genJsonQuery)
import           PgNamed ( (=?), queryNamed, PgNamedError )

-- Schema
----------------------------------------------------------------------

data Picture = Picture {
  id       :: Int,
  uuid     :: UUID,
  fileName :: Text,
  fileHash :: Text,
  url      :: Text,
  mimeType :: Text,
  tags     :: [Text]
} deriving (Generic,Show, Eq, FromRow)

instance FromJSON Picture where
  parseJSON (Object v) = do
    id        <-  v .: "id"
    uuid      <-  v .: "uuid"
    fileName  <-  v .: "file_name"
    fileHash  <-  v .: "file_hash"
    url       <-  v .: "url"
    mimeType  <-  v .: "mime_type"
    tags      <-  v .: "tags"
    return (Picture id uuid fileName fileHash url mimeType tags)

  parseJSON _ = empty

-- Queries
----------------------------------------------------------------------

data Order = ASC | DESC deriving Show

data IndexedField = ID | UUID | FileName

instance Show IndexedField where
  show ID       = "id"
  show UUID     = "uuid"
  show FileName = "file_name"

instance ToField IndexedField where
  toField = Plain . string8 . show

data OrderBy = OrderBy IndexedField Order | Random

instance ToField OrderBy where
  toField (OrderBy field ord) = Plain $ string8 $ show field ++ " " ++ show ord
  toField Random              = Plain $ string8 "random()"

----------------------------------------------------------------------
-- | Returns one picture
getBy :: (ToField a) => IndexedField -> a -> Connection -> IO (Either RecordError (Maybe Picture))
getBy field value conn = do
  $(genJsonQuery [qq|
    select p.id                   as id          -- Int
         , p.uuid                 as uuid        -- UUID
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
    group by p.id
  |]) conn >>= parseOne

getByUUID :: UUID -> Connection -> IO (Either RecordError (Maybe Picture))
getByUUID = getBy UUID

getByFileName :: Text -> Connection -> IO (Either RecordError (Maybe Picture))
getByFileName = getBy FileName

----------------------------------------------------------------------

data GetAllInput = GetAllInput {
  orderBy :: OrderBy,
  pagination :: Maybe PaginationInput
}

instance Defaults GetAllInput where
  def = GetAllInput (OrderBy ID DESC) Nothing

-- | Returns all pictures paginated with no filters
getAll :: GetAllInput -> Connection -> IO (Either RecordError [Picture])
getAll GetAllInput{..} conn =
  let PaginationParams {..} = parsePaginationInput pagination
  in $(genJsonQuery [qq|
    select p.id                   as id            -- Int
         , p.uuid                 as uuid          -- UUID
         , p.file_name            as file_name     -- Text
         , p.url                  as url           -- Text
         , p.mime_type            as mime_type     -- Text
         , p.file_hash            as file_hash     -- Text
         , array_agg(pts.tag)     as tags          -- [Text]
    from pictures p
    inner join picture_tags pts
      on pts.picture_uuid = p.uuid
    group by p.id
    order by ?                                     -- < orderBy
    limit ?                                        -- < limit
    offset ?                                       -- < offset
  |]) conn >>= parseMany

----------------------------------------------------------------------

data FindByTagsInput = FindByTagsInput {
  tags :: [Text],
  orderBy :: OrderBy,
  pagination :: Maybe PaginationInput
}

instance Defaults FindByTagsInput where
  def = FindByTagsInput [] (OrderBy ID DESC) Nothing

-- | Returns a list of pictures with any of the given tags
findByTags
  :: FindByTagsInput -> Connection -> IO (Either RecordError [Picture])
findByTags FindByTagsInput{..} conn =
  let PaginationParams {..} = parsePaginationInput pagination
  in parseResultNamed $ queryNamed conn findByTagsQuery [
    "tags"    =? tags,
    "orderBy" =? orderBy,
    "limit"   =? limit,
    "offset"  =? offset
  ]
