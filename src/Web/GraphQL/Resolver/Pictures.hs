{-# LANGUAGE DeriveAnyClass #-}

module Web.GraphQL.Resolver.Pictures
  ( Picture
  , PictureArgs
  , pictureResolver
  )
where

import qualified Model.Picture                 as DB
import           Model                          ( RecordError )
import           Env                            ( Connection )

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

data PictureArgs = PictureArgs
  { fileName      :: Text
  } deriving (Generic, GQLType)

-- Resolvers
----------------------------------------------------------------------

transform :: DB.Picture -> Picture
transform DB.Picture {..} = Picture { uuid, url, fileName, mimeType, tags }

mapRecord :: Either RecordError DB.Picture -> Either String Picture
mapRecord = mapBoth show transform

pictureResolver :: Connection -> PictureArgs -> IORes e Picture
pictureResolver conn PictureArgs { fileName } =
  liftEither . liftM mapRecord $ DB.getByFileName fileName conn
