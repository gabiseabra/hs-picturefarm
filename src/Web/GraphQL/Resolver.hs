module Web.GraphQL.Resolver
  ( rootResolver
  )
where

import           Env                            ( Connection )

import           GHC.Generics

import           Data.Morpheus.Types

import           Web.GraphQL.Types
import           Web.GraphQL.Resolver.Pictures

data Query m = Query
  { picture  :: PictureInput -> m (Maybe Picture)
  , pictures :: PicturesInput -> m [Picture]
  } deriving (Generic, GQLType)

rootResolver :: Connection -> GQLRootResolver IO () Query Undefined Undefined
rootResolver conn = GQLRootResolver { queryResolver
                                    , mutationResolver     = Undefined
                                    , subscriptionResolver = Undefined
                                    }
 where
  queryResolver =
    Query { picture = pictureResolver conn, pictures = picturesResolver conn }
