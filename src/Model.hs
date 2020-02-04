{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Model
  ( RecordError(..)
  , parseOne
  , parseMany
  , parseResultNamed
  )
where

import           Control.Monad
import           Control.Monad.Trans.Except     ( ExceptT
                                                , runExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )

import           Data.Either.Combinators
import           Data.Aeson
import           Data.Aeson.Types               ( parseJSON
                                                , parseEither
                                                , listValue
                                                )
import           Data.Typeable
import           Data.String                    ( IsString(..) )
import           Data.String.Conversions        ( ConvertibleStrings(..) )

import           PgNamed                        ( WithNamedError )
import           Database.PostgreSQL.Simple     ( Query )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )

-- Utilities
----------------------------------------------------------------------

instance ConvertibleStrings String Query where
  convertString = fromString

-- Postgres field parsers
----------------------------------------------------------------------

instance (ToField a) => ToField [a] where
  toField = toField . PGArray

instance (FromField a, Typeable a) => FromField [a] where
  fromField f v = fromPGArray <$> fromField f v

-- Model helpers
----------------------------------------------------------------------

data RecordError = RecordError String deriving (Show, Eq)

parseOne :: (FromJSON a) => [Value] -> IO (Either RecordError (Maybe a))
parseOne [] = return $ Right Nothing
parseOne (record : _) =
  return . mapBoth RecordError Just . parseEither parseJSON $ record

parseMany :: (FromJSON a) => [Value] -> IO (Either RecordError [a])
parseMany = return . mapLeft RecordError . mapM (parseEither parseJSON)

parseResultNamed :: (Show e) => ExceptT e IO a -> IO (Either RecordError a)
parseResultNamed = (liftM $ mapLeft $ RecordError . show) . runExceptT
