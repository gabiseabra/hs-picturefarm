{-# LANGUAGE DeriveAnyClass #-}

module GraphQL.Resolver
  ( rootResolver
  )
where

import           Env                            ( Connection )

import           GHC.Generics

import           Data.Morpheus.Types

import           GraphQL.Types
import           GraphQL.Resolver.Pictures

data Query m = Query
  { picture :: PictureArgs -> m Picture
  } deriving (Generic, GQLType)

rootResolver :: Connection -> GQLRootResolver IO () Query Undefined Undefined
rootResolver conn = GQLRootResolver { queryResolver
                                    , mutationResolver     = Undefined
                                    , subscriptionResolver = Undefined
                                    }
  where queryResolver = Query { picture = pictureResolver conn }
