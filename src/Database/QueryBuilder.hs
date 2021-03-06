{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Database.QueryBuilder
  ( ClauseType(..)
  , Clause(..)
  , QueryOptions(..)
  , Order(..)
  , OrderBy(..)
  )
where

import           GHC.Generics

import qualified Data.List                     as List
import qualified Data.ByteString               as BS
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as UUID
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
  convertString = UUID.toString

instance ConvertibleStrings UUID BS.ByteString where
  convertString = convertString . UUID.toString

-- Query builders
----------------------------------------------------------------------

-- | Helpers for building composable filters for PostgreSQL queries
-- with record structures that represent options for a statement
class QueryOptions a where
  filterableFields :: a -> [String]

  applyFilters :: String -> a -> [Clause]

  toFilters :: a -> [Clause]
  toFilters a = concatMap (`applyFilters` a) $ filterableFields a

  buildClause :: ClauseType -> a -> String
  buildClause JOIN = between "\n" . pickByType JOIN . toFilters
  buildClause WHERE =
    prefix "where" . between "and" . pickByType WHERE . toFilters
  buildClause CTE =
    prefix "with" . between ", " . pickByType CTE . toFilters

instance QueryOptions [Clause] where
  filterableFields _ = []
  applyFilters _ a = a
  toFilters a = a

extractQuery :: Clause -> String
extractQuery (Clause t q) = q

isFilterType :: ClauseType -> Clause -> Bool
isFilterType t (Clause t' _) = t == t'

pickByType t = map extractQuery . filter (isFilterType t)

prefix _ "" = ""
prefix p q  = p ++ " " ++ q

between str = List.intercalate (" " ++ str ++ " ")

----------------------------------------------------------------------

data ClauseType = JOIN | WHERE | CTE deriving (Eq)

data Clause = Clause ClauseType String

----------------------------------------------------------------------

data Order = ASC | DESC deriving Show

data OrderBy a = OrderBy a Order | Random

instance (Show a) => ToField (OrderBy a) where
  toField (OrderBy field ord) = Plain $ string8 $ show field ++ " " ++ show ord
  toField Random              = Plain $ string8 "random()"
