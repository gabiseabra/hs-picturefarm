{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Database.QueryBuilder
  ( ClauseType(..)
  , Filter(..)
  , buildClause
  )
where

import           GHC.Generics

import           Data.Data
import           Data.List                      ( concat
                                                , intersperse
                                                )
import           Data.String                    ( IsString(..) )
import           Data.String.Conversions        ( ConvertibleStrings(..) )

import           Database.PostgreSQL.Simple     ( Query )

-- Utilities
----------------------------------------------------------------------

instance ConvertibleStrings String Query where
  convertString = fromString

-- Query builders
--
-- Helpers for building composable filters for PostgreSQL queries
----------------------------------------------------------------------

data ClauseType = JOIN | WHERE deriving (Eq)

data Filter = Filter ClauseType String

extractQuery (Filter t q) = q

isFilterType t (Filter t' _) = t == t'

pickByType t = map extractQuery . filter (isFilterType t)

prefix _ "" = ""
prefix p q  = p ++ " " ++ q

between str = concat . intersperse (" " ++ str ++ " ")

-- | Builds a SQL clause
buildClause :: ClauseType -> [Filter] -> String
buildClause JOIN = between "\n" . pickByType JOIN
buildClause WHERE = prefix "where" . between "and" . pickByType WHERE
