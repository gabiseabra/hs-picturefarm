module Model.Pagination
  ( PaginationInput(..)
  , PaginationParams(..)
  , parsePaginationInput
  )
where

import           Model

import           Data.Maybe
import           Data.String.QM

import           Database.PostgreSQL.Simple.TypedQuery


-- Schema
----------------------------------------------------------------------

data PaginationInput = PaginationInput {
  page     :: Maybe Int,
  pageSize :: Maybe Int
} deriving (Show, Eq)

data PaginationParams = PaginationParams {
  limit     :: Int,
  offset    :: Int
} deriving (Show, Eq)

lim a b c = max a $ min b c

parsePaginationInput :: Maybe PaginationInput -> PaginationParams
parsePaginationInput Nothing =
  parsePaginationInput $ Just (PaginationInput Nothing Nothing)
parsePaginationInput (Just PaginationInput {..}) =
  let limit  = lim 1 100 $ fromMaybe 100 pageSize
      offset = (* limit) $ max 0 $ (subtract 1) $ fromMaybe 1 page
  in  PaginationParams { limit, offset }
