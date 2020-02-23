module Model.Picture
  ( Picture(..)
  , OrderBy(..)
  , Order(..)
  , IndexedField(..)
  , FindPicturesInput(..)
  , insertPicture
  , updatePicture
  , getPictureBy
  , findPictures
  )
where

import           GHC.Generics

import           Database.QueryBuilder
import           Model
import           Model.Pagination
import           Model.Picture.Query
import           Service.Cloudinary             ( CloudinaryResource(..) )
import           Control.Monad
import           Control.Monad.Error.Class      ( liftEither )
import           Control.Applicative            ( empty )

import           Data.Default.Class
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as UUID
import           Data.String.QM
import           Data.Tuple.Curry               ( uncurryN )

import           Database.PostgreSQL.Simple     ( Only(..)
                                                , Connection
                                                , fromOnly
                                                , returning
                                                , execute
                                                , executeMany
                                                , withTransaction
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..) )
import           PgNamed                        ( (=?)
                                                , queryNamed
                                                )

-- Schema
----------------------------------------------------------------------

data Picture = Picture
  { id           :: Int
  , uuid         :: UUID.UUID
  , fileName     :: Text
  , fileHash     :: Text
  , url          :: Text
  , resourceType :: Text
  , mimeType     :: Text
  , tags         :: [Text]
  } deriving (Generic, Show, Eq, FromRow)

instance CloudinaryResource Picture where
  cdnPublicId     = url
  cdnResourceType = resourceType

-- Queries
----------------------------------------------------------------------

insertPicture :: Connection -> Picture -> IO (Int, UUID)
insertPicture conn Picture {..} = do
  [(rid, uuid)] :: [(Int, UUID)] <- returning
    conn
    insertPictureQuery
    [(fileName, fileHash, url, resourceType, mimeType)]
  _ <- executeMany conn insertPictureTagQuery $ unnest uuid tags
  return (rid, uuid)

updatePicture :: Connection -> Picture -> IO ()
updatePicture conn Picture {..} = withTransaction conn $ do
  execute conn
          updatePictureQuery
          (fileName, fileHash, url, resourceType, mimeType, uuid)
  execute conn deletePictureTagsQuery (Only uuid)
  executeMany conn insertPictureTagQuery $ unnest uuid tags
  return ()

unnest :: UUID -> [Text] -> [(UUID, Text)]
unnest uuid tags = flip zip tags $ take (length tags) $ repeat uuid

-- | Get picture by some unique field
----------------------------------------------------------------------
getPictureBy
  :: (ToField a) => Connection -> IndexedField -> a -> IO (Maybe Picture)
getPictureBy conn field value = maybeParseOne
  $ queryNamed conn getPictureByQuery ["field" =? field, "value" =? value]

-- | Filter pictures
----------------------------------------------------------------------

findPictures :: Connection -> FindPicturesInput -> IO [Picture]
findPictures conn input@FindPicturesInput {..} =
  let PaginationParams {..} = parsePaginationInput pagination
  in  parseMany $ queryNamed
        conn
        (findPictureQuery input)
        [ "tags" =? tags
        , "resourceType" =? resourceType
        , "orderBy" =? orderBy
        , "limit" =? limit
        , "offset" =? offset
        ]

data FindPicturesInput = FindPicturesInput
  { tags         :: Maybe [Text]
  , resourceType :: Maybe Text
  , orderBy      :: OrderBy IndexedField
  , pagination   :: Maybe PaginationInput
  }

instance QueryOptions FindPicturesInput where
  filterableFields _ = ["tags", "resourceType"]

  applyFilters "tags" FindPicturesInput { tags = Just _ } = [tagsFilter]
  applyFilters "resourceType" FindPicturesInput { resourceType = Just _ } =
    [resourceTypeFilter]
  applyFilters _ _ = []

instance Default FindPicturesInput where
  def = FindPicturesInput Nothing Nothing (OrderBy ID DESC) Nothing
