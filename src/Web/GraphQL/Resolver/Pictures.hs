module Web.GraphQL.Resolver.Pictures
  ( Picture
  , PictureInput
  , PicturesInput
  , pictureResolver
  , picturesResolver
  )
where

import           Model.Picture                  ( FindByTagsInput(..)
                                                , GetAllInput(..)
                                                , getAll
                                                , findByTags
                                                , getByFileName
                                                )
import qualified Model.Picture                 as DB
                                                ( Picture(..) )
import           Model.Pagination               ( PaginationInput )
import           Model                          ( RecordError )
import           Env                            ( Connection )
import           Defaults

import           GHC.Generics

import           Control.Monad.Trans.Class
import           Control.Monad

import           Data.Either.Combinators
import           Data.UUID
import           Data.Text                      ( Text )
import           Data.Morpheus.Kind
import           Data.Morpheus.Types

-- GraphQL types
----------------------------------------------------------------------

data Picture = Picture
  { uuid      :: UUID
  , url       :: Text
  , fileName  :: Text
  , mimeType  :: Text
  , tags      :: [Text]
  } deriving (Generic, GQLType)

data PictureInput = PictureArgs
  { fileName      :: Text
  } deriving (Generic, GQLType)

data PicturesInput = PicturesArgs
  { tags       :: Maybe [Text],
    pagination :: Maybe PaginationInput
  } deriving (Generic, GQLType)

-- Resolvers
----------------------------------------------------------------------

transform :: DB.Picture -> Picture
transform DB.Picture {..} = Picture { uuid, url, fileName, mimeType, tags }

transformM :: (Maybe DB.Picture) -> Maybe Picture
transformM = liftM transform

mapRecord
  :: Either RecordError (Maybe DB.Picture) -> Either String (Maybe Picture)
mapRecord = mapBoth show transformM

mapRecords :: Either RecordError [DB.Picture] -> Either String [Picture]
mapRecords = mapBoth show (map transform)

pictureResolver :: Connection -> PictureInput -> IORes e (Maybe Picture)
pictureResolver conn PictureArgs { fileName } =
  liftEither . liftM mapRecord $ getByFileName fileName conn

picturesResolver :: Connection -> PicturesInput -> IORes e [Picture]
picturesResolver conn PicturesArgs { tags = Nothing, pagination } =
  liftEither . liftM mapRecords $ getAll (def { pagination } :: GetAllInput)
                                         conn
picturesResolver conn PicturesArgs { tags = Just tags, pagination } =
  liftEither . liftM mapRecords $ findByTags
    (def { tags, pagination } :: FindByTagsInput)
    conn
