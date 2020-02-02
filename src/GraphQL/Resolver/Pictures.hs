{-# LANGUAGE DeriveAnyClass #-}

module GraphQL.Resolver.Pictures
  ( Picture
  , PictureArgs
  , pictureResolver
  )
where

import qualified Model.Picture                 as DB
import           Model                          ( RecordError )
import           Env                 ( Connection )

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
  } deriving (Generic, GQLType)

data PictureArgs = PictureArgs
  { uuid      :: UUID
  } deriving (Generic, GQLType)

-- Resolvers
----------------------------------------------------------------------

mapRecord :: (GQLType b) => (a -> b) -> Either RecordError a -> Either String b
mapRecord = mapBoth show

transform :: DB.Picture -> Picture
transform picture = Picture { uuid = DB.uuid picture }

pictureResolver :: Connection -> PictureArgs -> IORes e Picture
pictureResolver conn PictureArgs { uuid } =
  liftEither . liftM (mapRecord transform) $ DB.getByUuid uuid conn
