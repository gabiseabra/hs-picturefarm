module Model.Picture
  ( RecordError(..)
  , Picture(..)
  , OrderBy(..)
  , Order(..)
  , IndexedField(..)
  , FindPicturesInput(..)
  , getPictureBy
  , findPictures
  )
where

import           GHC.Generics

import           Database.QueryBuilder
import           Model
import           Model.Pagination
import           Model.Picture.Query

import           Control.Monad
import           Control.Monad.Error.Class      ( liftEither )
import           Control.Applicative            ( empty )

import           Data.Default.Class
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Data.String.QM
import           Data.Tuple.Curry               ( uncurryN )

import           Database.PostgreSQL.Simple     ( Only(..)
                                                , Connection
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..) )
import           PgNamed                        ( (=?)
                                                , queryNamed
                                                , PgNamedError
                                                )

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
} deriving (Generic, Show, Eq, FromRow)

-- Queries
----------------------------------------------------------------------

getPictureBy
  :: (ToField a)
  => Connection
  -> IndexedField
  -> a
  -> IO (Either RecordError (Maybe Picture))
getPictureBy conn field value = parseOne
  $ queryNamed conn getPictureByQuery ["field" =? field, "value" =? value]

----------------------------------------------------------------------

findPictures
  :: Connection -> FindPicturesInput -> IO (Either RecordError [Picture])
findPictures conn input@FindPicturesInput {..} =
  let PaginationParams {..} = parsePaginationInput pagination
  in  parseMany $ queryNamed
        conn
        (findPictureQuery input)
        [ "tags" =? tags
        , "orderBy" =? orderBy
        , "limit" =? limit
        , "offset" =? offset
        ]

data FindPicturesInput = FindPicturesInput {
  tags :: Maybe [Text],
  orderBy :: OrderBy IndexedField,
  pagination :: Maybe PaginationInput
}

instance QueryOptions FindPicturesInput where
  filterableFields _ = ["tags"]

  applyFilters "tags" FindPicturesInput { tags = Just _ } = [tagsFilter]
  applyFilters _      _ = []

instance Default FindPicturesInput where
  def = FindPicturesInput Nothing (OrderBy ID DESC) Nothing
