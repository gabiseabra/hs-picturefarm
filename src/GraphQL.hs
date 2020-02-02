module GraphQL
  ( api
  )
where

import           Env                            ( Connection )
import           GraphQL.Resolver               ( rootResolver )

import           Data.Morpheus                  ( interpreter )
import           Data.ByteString.Lazy.Char8     ( ByteString )

api :: Connection -> ByteString -> IO ByteString
api conn = interpreter $ rootResolver conn
