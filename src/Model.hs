module Model (
  parseOne,
  parseMany
) where

import Control.Monad

import Data.Aeson
import Data.Aeson.Types (parseJSON, parseJSONList, parseEither, listValue)
import Data.Typeable (Typeable)

import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.Types (PGArray(..))

-- Postgres field parsers
----------------------------------------------------------------------

instance (ToField a) => ToField [a] where
  toField = toField . PGArray

instance (FromField a, Typeable a) => FromField [a] where
  fromField f v = fromPGArray <$> fromField f v

-- Model helpers
----------------------------------------------------------------------

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

parseOne :: (FromJSON a) => [Value] -> IO (Maybe (Either String a))
parseOne = return . liftM(parseEither parseJSON) . safeHead

parseMany :: (FromJSON a) => [Value] -> IO (Either String [a])
parseMany = return . mapM (parseEither parseJSON)
