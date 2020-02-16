module Web.GraphQL.Resolver.Pictures
  ( Picture
  , PictureInput
  , RandomPictureInput
  , PicturesInput
  , pictureResolver
  , randomPictureResolver
  , picturesResolver
  )
where

import           GHC.Generics

import           Model.Picture           hiding ( Picture )
import qualified Model.Picture                 as DB
                                                ( Picture(..) )
import           Model.Pagination               ( PaginationInput(..) )
import           Model                          ( RecordError )
import           Env                            ( Connection )

import           Data.Default.Class
import           Data.Either.Combinators
import           Data.UUID
import           Data.Text                      ( Text )
import           Data.Morpheus.Kind
import           Data.Morpheus.Types

import           Control.Error.Safe             ( headZ )
import           Control.Monad.Trans.Class
import           Control.Monad

-- GraphQL types
----------------------------------------------------------------------

data Picture = Picture
  { uuid      :: UUID
  , url       :: Text
  , fileName  :: Text
  , mimeType  :: Text
  , tags      :: [Text]
  } deriving (Generic, GQLType)

data PictureInput = PictureInput
  { fileName      :: Text
  } deriving (Generic, GQLType)

data RandomPictureInput = RandomPictureInput
  { tags      :: Maybe [Text]
  } deriving (Generic, GQLType)

data PicturesInput = PicturesInput
  { tags       :: Maybe [Text],
    pagination :: Maybe PaginationInput
  } deriving (Generic, GQLType)

-- Resolvers
----------------------------------------------------------------------

pictureResolver :: Connection -> PictureInput -> IORes e (Maybe Picture)
pictureResolver conn PictureInput { fileName } =
  liftEither . fmap mapOne $ getPictureBy conn FileName fileName

randomPictureResolver
  :: Connection -> RandomPictureInput -> IORes e (Maybe Picture)
randomPictureResolver conn RandomPictureInput { tags } =
  let pagination = Just PaginationInput { page = Just 1, pageSize = Just 1 }
      input = def { tags, orderBy = Random, pagination } :: FindPicturesInput
  in  liftEither . fmap (mapManyWith headZ) $ findPictures conn input

picturesResolver :: Connection -> PicturesInput -> IORes e [Picture]
picturesResolver conn PicturesInput { tags, pagination } =
  let input = def { tags, pagination } :: FindPicturesInput
  in  liftEither . fmap mapMany $ findPictures conn input

----------------------------------------------------------------------

mapOneWith
  :: (Maybe Picture -> a)
  -> Either RecordError (Maybe DB.Picture)
  -> Either String a
mapOneWith = mapBoth show . (. transformM)

mapOne = mapOneWith Prelude.id

mapManyWith
  :: ([Picture] -> a) -> Either RecordError [DB.Picture] -> Either String a
mapManyWith = mapBoth show . (. map transform)

mapMany = mapManyWith Prelude.id

transform :: DB.Picture -> Picture
transform DB.Picture {..} = Picture { uuid, url, fileName, mimeType, tags }

transformM :: (Maybe DB.Picture) -> Maybe Picture
transformM = fmap transform
