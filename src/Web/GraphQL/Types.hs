module Web.GraphQL.Types where

import           Model.Pagination               ( PaginationInput )

import           GHC.Generics

import           Control.Arrow
import           Data.Either.Combinators

import           Data.UUID
import qualified Data.Text                     as T
import           Data.Morpheus.Kind
import           Data.Morpheus.Types

-- Scalar types
----------------------------------------------------------------------

instance GQLScalar UUID where
  parseValue (String value) =
    maybeToRight "Invalid UUID" . fromString . T.unpack $ value

  serialize = String . T.pack . toString

instance GQLType UUID where
  type KIND UUID = SCALAR

-- Object types
----------------------------------------------------------------------

instance GQLType PaginationInput where
  type KIND PaginationInput = INPUT
