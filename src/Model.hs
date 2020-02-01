module Model
  ( RecordError(..)
  , parseOne
  , parseMany
  )
where

import           Control.Monad

import           Data.Either.Combinators
import           Data.Aeson
import           Data.Aeson.Types               ( parseJSON
                                                , parseJSONList
                                                , parseEither
                                                , listValue
                                                )
import           Data.Typeable

import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )

-- Postgres field parsers
----------------------------------------------------------------------

instance (ToField a) => ToField [a] where
  toField = toField . PGArray

instance (FromField a, Typeable a) => FromField [a] where
  fromField f v = fromPGArray <$> fromField f v

-- Model helpers
----------------------------------------------------------------------

data RecordError = NotFound | ParseError String deriving (Show, Eq)

parseOne :: (FromJSON a) => [Value] -> IO (Either RecordError a)
parseOne [] = return $ Left NotFound
parseOne (record : _) =
  return . mapLeft ParseError . parseEither parseJSON $ record

parseMany :: (FromJSON a) => [Value] -> IO (Either RecordError [a])
parseMany = return . mapLeft ParseError . mapM (parseEither parseJSON)
