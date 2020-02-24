module Web.Middleware.Router
  ( module Network.Wai.Middleware.Router
  , match
  , dir
  )
where

import qualified Data.Text                     as T
import qualified Data.List                     as List

import           Network.Wai                    ( Application
                                                , Middleware
                                                )
import           Network.Wai.Middleware.Rewrite ( rewritePureWithQueries )
import           Network.Wai.Middleware.Router  ( Route
                                                , router
                                                )

-- | Match the beginning of a path
match :: T.Text -> Application -> Route
match path app =
  let ps = pathPieces path
  in  (\xs -> case List.stripPrefix ps (filter ("" /=) xs) of
        Just path -> Just $ rewrite path $ app
        Nothing   -> Nothing
      )

-- | Match path exactly
dir :: T.Text -> Application -> Route
dir path app =
  let ps = pathPieces path
  in  (\xs -> if ps == xs then Just $ rewrite ["/"] $ app else Nothing)

rewrite :: [T.Text] -> Middleware
rewrite path = rewritePureWithQueries $ \(_, query) -> const (path, query)

-- | Pieces of a URL path.
pathPieces :: T.Text -> [T.Text]
pathPieces path = filter ("" /=) $ T.splitOn "/" path
