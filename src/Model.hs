module Model
  ( parseOne
  , parseMany
  , maybeParseOne
  )
where

import           Control.Exception              ( Exception
                                                , throwIO
                                                )
import           Control.Monad                  ( (<=<) )
import           Control.Monad.Except           ( throwError )
import           Control.Monad.Trans.Except     ( ExceptT
                                                , runExceptT
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Error.Safe             ( headZ )

import           Data.Typeable

import           Database.PostgreSQL.Simple     ( FromRow(..) )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )
import           PgNamed                        ( PgNamedError )

-- Generic PostgreSQL field parsers
----------------------------------------------------------------------

instance (ToField a) => ToField [a] where
  toField = toField . PGArray

instance (FromField a, Typeable a) => FromField [a] where
  fromField f v = fromPGArray <$> fromField f v

instance Exception PgNamedError

-- Model helpers
----------------------------------------------------------------------

maybeParseOne :: (Exception e) => ExceptT e IO [a] -> IO (Maybe a)
maybeParseOne = return . headZ <=< parseMany

parseOne :: (Exception e) => ExceptT e IO [a] -> IO a
parseOne = return . head <=< parseMany

parseMany :: (Exception e) => ExceptT e IO [a] -> IO [a]
parseMany = either (liftIO . throwIO) pure <=< runExceptT
