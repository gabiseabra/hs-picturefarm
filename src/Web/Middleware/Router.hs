module Web.Middleware.Router
  ( Route
  , router
  , match
  , dir
  )
where

import qualified Data.Text                     as T
import qualified Data.List                     as List

import           Network.Wai                    ( Application
                                                , Middleware
                                                , pathInfo
                                                )
import           Network.Wai.Middleware.Rewrite ( rewritePureWithQueries )

-- WAI Router
-- https://github.com/mdmarek/wai-router/blob/master/Network/Wai/Middleware/Router.hs
--------------------------------------------------------------------------------

-- | Alias for a function which maps path pieces to applications.
type Route = ([T.Text] -> Maybe Application)

-- | Router for mapping paths to applications.
-- 
-- For example:
-- 
-- > router [ dir "/foo" fooApp
-- >        , dir "/api" apiApp 
-- >        ] defaultApp
router :: [Route] -> Application -> Application
router routes d req = case router' (pathInfo req) routes of
  Nothing -> d req
  Just a  -> a req

-- | First matching paths' application, nothing otherwise.
router' :: [T.Text] -> [Route] -> Maybe Application
router' _  []       = Nothing
router' ps (r : rs) = case r ps of
  Nothing -> router' ps rs
  Just a  -> Just a

-- Route constructors
--------------------------------------------------------------------------------

-- | Match the beginning of a path
match :: T.Text -> Application -> Route
match path app =
  let ps = pathPieces path
  in  (\xs -> case List.stripPrefix ps (stripSlash xs) of
        Just path -> Just $ rewrite path $ app
        Nothing   -> Nothing
      )

-- | Match path exactly
dir :: T.Text -> Application -> Route
dir path app =
  let ps = pathPieces path
  in  (\xs -> if ps == xs then Just $ rewrite [] $ app else Nothing)

rewrite :: [T.Text] -> Middleware
rewrite path =
  rewritePureWithQueries $ \(o, query) -> const (stripSlash path, query)

-- | Pieces of a URL path.
pathPieces :: T.Text -> [T.Text]
pathPieces path = stripSlash $ T.splitOn "/" path

-- | Strip empty slashes from path. e.g: my//path/ -> my/path
stripSlash = filter ("" /=)
