module Model
  ( RecordError(..)
  , parseOne
  , parseMany
  )
where

import           Control.Monad
import           Control.Monad.Trans.Except     ( ExceptT
                                                , runExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Error.Safe             ( headZ )

import           Data.Either.Combinators
import           Data.Typeable

import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )

-- Generic PostgreSQL field parsers
----------------------------------------------------------------------

instance (ToField a) => ToField [a] where
  toField = toField . PGArray

instance (FromField a, Typeable a) => FromField [a] where
  fromField f v = fromPGArray <$> fromField f v

-- Model helpers
----------------------------------------------------------------------

data RecordError = RecordError String deriving (Show, Eq)

parseOne :: (Show e) => ExceptT e IO [a] -> IO (Either RecordError (Maybe a))
parseOne = liftM (mapRight headZ) . parseMany

parseMany :: (Show e) => ExceptT e IO [a] -> IO (Either RecordError [a])
parseMany = (liftM $ mapLeft $ RecordError . show) . runExceptT
