{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Database.QueryBuilder
  ( ClauseType(..)
  , Filter(..)
  , QueryOptions(..)
  , Order(..)
  , OrderBy(..)
  )
where

import           GHC.Generics

import qualified Data.List                     as List
import qualified Data.ByteString               as BS
import           Data.UUID                      ( UUID )
import           Data.String                    ( IsString(..) )
import           Data.String.Conversions        ( ConvertibleStrings(..) )
import           Data.ByteString.Builder        ( string8 )

import           Database.PostgreSQL.Simple     ( Query )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(..)
                                                , ToField(..)
                                                )

-- Utilities
----------------------------------------------------------------------

instance ConvertibleStrings String Query where
  convertString = fromString

instance ConvertibleStrings UUID String where
  convertString = show

instance ConvertibleStrings UUID BS.ByteString where
  convertString = convertString . show

-- Query builders
----------------------------------------------------------------------

-- | Helpers for building composable filters for PostgreSQL queries
-- with record structures that represent options for a statement
class QueryOptions a where
  filterableFields :: a -> [String]

  applyFilters :: String -> a -> [Filter]

  toFilters :: a -> [Filter]
  toFilters a = concatMap (`applyFilters` a) $ filterableFields a

  buildClause :: ClauseType -> a -> String
  buildClause JOIN = between "\n" . pickByType JOIN . toFilters
  buildClause WHERE =
    prefix "where" . between "and" . pickByType WHERE . toFilters

instance QueryOptions [Filter] where
  filterableFields _ = []
  applyFilters _ a = a
  toFilters a = a

extractQuery (Filter t q) = q

isFilterType t (Filter t' _) = t == t'

pickByType t = map extractQuery . filter (isFilterType t)

prefix _ "" = ""
prefix p q  = p ++ " " ++ q

between str = List.concat . List.intersperse (" " ++ str ++ " ")

----------------------------------------------------------------------

data ClauseType = JOIN | WHERE deriving (Eq)

data Filter = Filter ClauseType String

----------------------------------------------------------------------

data Order = ASC | DESC deriving Show

data OrderBy a = OrderBy a Order | Random

instance (Show a) => ToField (OrderBy a) where
  toField (OrderBy field ord) = Plain $ string8 $ show field ++ " " ++ show ord
  toField Random              = Plain $ string8 "random()"
